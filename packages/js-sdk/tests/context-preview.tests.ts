/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * qs-02 -- Context.setPreview() integration tests (RED phase).
 *
 * Owns: AC3 (render half -- preview experience resolved only via the `?exp=` fetch),
 * AC5 (zero-trace, Node transport), AC6 (isolation across contexts on the same SDK
 * instance), AC7 (inert-on-bad-input, unknown-experience half).
 *
 * `Context.setPreview()` does not exist yet -- every call below is made through an
 * `as any` cast so the file compiles, and the calls themselves fail at runtime
 * (method undefined), which is the expected RED signal. `enableStorage` similarly
 * does not exist yet on `BucketingAttributes` / `DataManager` -- this file never sets
 * it directly; it is exercised indirectly, end-to-end, through `Context.setPreview()`
 * once implemented (see spec item 1 in the driving task).
 */
import 'mocha';
import {expect} from 'chai';
import http from 'http';

import {BucketingManager as bm} from '@convertcom/js-sdk-bucketing';
import {RuleManager as rm} from '@convertcom/js-sdk-rules';
import {EventManager as em} from '@convertcom/js-sdk-event';
import {ApiManager as am} from '@convertcom/js-sdk-api';
import {DataManager as dm} from '@convertcom/js-sdk-data';
import {ExperienceManager as exm} from '@convertcom/js-sdk-experience';
import {FeatureManager as fm} from '../src/feature-manager';
import {SegmentsManager as sm} from '@convertcom/js-sdk-segments';
import {Context as c} from '../src/context';
import {defaultConfig} from '../src/config/default';
import {objectDeepMerge} from '@convertcom/js-sdk-utils';
import {SystemEvents} from '@convertcom/js-sdk-enums';
import {
  Config as ConfigType,
  ConfigExperience,
  ExperienceVariationConfig,
  ExperienceStatuses,
  VariationStatuses
} from '@convertcom/js-sdk-types';

// --- Fixed test infra: dedicated host/port for this file (never shared with
// context.tests.ts/core.tests.ts/feature-manager.tests.ts, all on 8090) ---
const host = 'http://localhost';
const port = 8096;
const release_timeout = 300;
const wait_margin = 400;
const test_timeout = release_timeout + wait_margin + 3000;
const batch_size = 5;

const ACCOUNT_ID = 'preview-ctx-account';
const PROJECT_ID = 'preview-ctx-project';
const GOAL_ID = 'preview-ctx-goal-id';
const GOAL_KEY = 'preview-ctx-goal-key';

const sleep = (ms: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, ms));

// --- Fixture builders -------------------------------------------------------

function makeVariation(
  id: string,
  overrides: Partial<ExperienceVariationConfig> = {}
): ExperienceVariationConfig {
  return {
    id,
    key: `${id}-key`,
    name: `Variation ${id}`,
    status: VariationStatuses.RUNNING,
    is_baseline: false,
    traffic_allocation: 50,
    changes: [],
    ...overrides
  } as unknown as ExperienceVariationConfig;
}

// No `locations`/`site_area`/`audiences` on purpose: combined with
// `ignoreLocationProperties: true` on the call site, this experience always
// matches deterministically without needing rule-matching fixtures.
function makeExperience(
  id: string,
  key: string,
  variationIds: string[],
  overrides: Partial<ConfigExperience> = {}
): ConfigExperience {
  return {
    id,
    key,
    name: `Experience ${key}`,
    type: 'a/b_fullstack',
    status: ExperienceStatuses.ACTIVE,
    variations: variationIds.map((variationId) => makeVariation(variationId)),
    ...overrides
  } as unknown as ConfigExperience;
}

function makeGoal(): Record<string, any> {
  return {id: GOAL_ID, key: GOAL_KEY, name: 'Preview Context Goal'};
}

class SpyDataStore {
  data: Record<string, any> = {};
  setCallCount = 0;
  get(key: string) {
    if (!key) return this.data;
    return this.data[key.toString()];
  }
  set(key: string, value: any) {
    this.setCallCount++;
    if (!key) throw new Error('Invalid DataStore key!');
    this.data[key.toString()] = value;
  }
}

function createSpyLogger() {
  const calls: Array<{method: string; args: any[]}> = [];
  const record =
    (method: string) =>
    (...args: any[]) => {
      calls.push({method, args});
    };
  return {
    log: record('log'),
    trace: record('trace'),
    debug: record('debug'),
    info: record('info'),
    warn: record('warn'),
    error: record('error'),
    addClient: () => undefined,
    setClientLevel: () => undefined,
    get calls() {
      return calls;
    },
    get warnings() {
      return calls.filter((call) => call.method === 'warn');
    }
  };
}

// --- SDK / Context factories -------------------------------------------------

let sdkCounter = 0;

interface Sdk {
  dataManager: InstanceType<typeof dm>;
  experienceManager: InstanceType<typeof exm>;
  featureManager: InstanceType<typeof fm>;
  segmentsManager: InstanceType<typeof sm>;
  apiManager: InstanceType<typeof am>;
  eventManager: InstanceType<typeof em>;
  config: ConfigType;
  logger: ReturnType<typeof createSpyLogger>;
}

function makeSdk(
  data: Record<string, any>,
  options: {dataStore?: SpyDataStore} = {}
): Sdk {
  sdkCounter += 1;
  const logger = createSpyLogger();
  const configuration = objectDeepMerge({}, defaultConfig, {
    api: {
      endpoint: {
        config: `${host}:${port}`,
        track: `${host}:${port}`
      }
    },
    events: {
      batch_size,
      release_interval: release_timeout
    },
    sdkKey: `preview-ctx-sdk-key-${sdkCounter}`,
    data,
    ...(options.dataStore ? {dataStore: options.dataStore} : {})
  }) as unknown as ConfigType;

  const bucketingManager = new bm(configuration);
  const ruleManager = new rm(configuration);
  const eventManager = new em(configuration, {loggerManager: logger});
  const apiManager = new am(configuration, {eventManager, loggerManager: logger});
  const dataManager = new dm(
    configuration,
    {bucketingManager, ruleManager, eventManager, apiManager, loggerManager: logger},
    {asyncStorage: false}
  );
  const experienceManager = new exm(configuration, {dataManager, loggerManager: logger});
  const featureManager = new fm(configuration, {dataManager, loggerManager: logger});
  const segmentsManager = new sm(configuration, {
    dataManager,
    ruleManager,
    loggerManager: logger
  });

  return {
    dataManager,
    experienceManager,
    featureManager,
    segmentsManager,
    apiManager,
    eventManager,
    config: configuration,
    logger
  };
}

function makeContext(sdk: Sdk, visitorId: string): any {
  return new c(sdk.config, visitorId, {
    eventManager: sdk.eventManager,
    experienceManager: sdk.experienceManager,
    featureManager: sdk.featureManager,
    segmentsManager: sdk.segmentsManager,
    dataManager: sdk.dataManager,
    apiManager: sdk.apiManager,
    loggerManager: sdk.logger
  });
}

// Wraps `dataManager.putData` with a call counter, mirroring the
// `apiManager.enqueue` wrap in data-manager-preview-decision.tests.ts (SDK-4) --
// the established pattern in this repo for proving "zero store writes" without a
// mocking library dependency.
function wrapPutData(dataManager: InstanceType<typeof dm>): {calls: number} {
  const tracker = {calls: 0};
  const original = dataManager.putData.bind(dataManager);
  (dataManager as any).putData = (...args: Parameters<typeof dataManager.putData>) => {
    tracker.calls++;
    return (original as any)(...args);
  };
  return tracker;
}

function countEventFires(
  eventManager: InstanceType<typeof em>,
  eventName: SystemEvents
): {count: number} {
  const tracker = {count: 0};
  eventManager.on(eventName, () => {
    tracker.count++;
  });
  return tracker;
}

// --- Scenario fixture: one preview (draft, ?exp=-only) experience plus one
// normal (shared-config) experience, uniquely namespaced per test via `prefix`
// so the process-wide getConfigByExperience() memoization (60s TTL, keyed by
// sdkKey+experienceId) can never leak state across tests. ---

interface Scenario {
  previewExperienceId: string;
  previewExperienceKey: string;
  targetVariationId: string;
  otherVariationId: string;
  normalExperienceId: string;
  normalExperienceKey: string;
  sdk: Sdk;
}

function buildScenario(
  prefix: string,
  previewResponses: Map<string, Record<string, any>>,
  options: {dataStore?: SpyDataStore; registerPreview?: boolean} = {}
): Scenario {
  const previewExperienceId = `${prefix}-preview-exp`;
  const previewExperienceKey = `${prefix}-preview-exp-key`;
  const targetVariationId = `${prefix}-preview-var-target`;
  const otherVariationId = `${prefix}-preview-var-other`;
  const normalExperienceId = `${prefix}-normal-exp`;
  const normalExperienceKey = `${prefix}-normal-exp-key`;

  const previewExperience = makeExperience(
    previewExperienceId,
    previewExperienceKey,
    [targetVariationId, otherVariationId],
    {status: ExperienceStatuses.DRAFT}
  );
  const normalExperience = makeExperience(normalExperienceId, normalExperienceKey, [
    `${prefix}-normal-var-a`,
    `${prefix}-normal-var-b`
  ]);

  if (options.registerPreview !== false) {
    previewResponses.set(previewExperienceId, {
      account_id: ACCOUNT_ID,
      project: {id: PROJECT_ID},
      experiences: [previewExperience]
    });
  }

  const sdk = makeSdk(
    {
      account_id: ACCOUNT_ID,
      project: {id: PROJECT_ID},
      experiences: [normalExperience],
      goals: [makeGoal()]
    },
    {dataStore: options.dataStore}
  );

  return {
    previewExperienceId,
    previewExperienceKey,
    targetVariationId,
    otherVariationId,
    normalExperienceId,
    normalExperienceKey,
    sdk
  };
}

describe('Context.setPreview() preview integration (RED)', function () {
  let server: http.Server;
  let previewResponses: Map<string, Record<string, any>>;
  let trackHits: string[];

  // eslint-disable-next-line mocha/no-hooks-for-single-case
  before(function () {
    server = http.createServer((request, res) => {
      const parsedUrl = new URL(request.url, `${host}:${port}`);
      if (parsedUrl.pathname.startsWith('/track/')) {
        trackHits.push(parsedUrl.pathname);
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end('{}');
        return;
      }
      if (parsedUrl.pathname.startsWith('/config/')) {
        const exp = parsedUrl.searchParams.get('exp');
        const body = (exp && previewResponses.get(exp)) || {
          account_id: ACCOUNT_ID,
          project: {id: PROJECT_ID},
          experiences: []
        };
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end(JSON.stringify(body));
        return;
      }
      res.writeHead(404);
      res.end();
    });
    server.listen(port);
  });

  // eslint-disable-next-line mocha/no-hooks-for-single-case
  after(function () {
    server.closeAllConnections();
    server.close();
  });

  beforeEach(function () {
    previewResponses = new Map();
    trackHits = [];
  });

  describe('AC3 -- render: preview experience resolved only via the ?exp= fetch', function () {
    it('runExperience returns the requested variation for an experience present ONLY via ?exp= (draft, not in shared config)', async function () {
      this.timeout(test_timeout);
      const scenario = buildScenario('ac3', previewResponses);
      const context = makeContext(scenario.sdk, 'visitor-ac3');

      await context.setPreview({
        experienceId: scenario.previewExperienceId,
        variationId: scenario.targetVariationId
      });
      const result = context.runExperience(scenario.previewExperienceKey);

      expect(result, 'preview decision').to.be.an('object');
      expect(result.experienceId).to.equal(scenario.previewExperienceId);
      expect(result.experienceKey).to.equal(scenario.previewExperienceKey);
      expect(result.id).to.equal(scenario.targetVariationId);
    });
  });

  describe('AC5 -- zero-trace: no /track requests and no store writes across the preview lifecycle (Node)', function () {
    it('produces zero track requests, zero store writes, and zero bucketing/conversion event fires', async function () {
      this.timeout(test_timeout);
      const dataStore = new SpyDataStore();
      const scenario = buildScenario('ac5', previewResponses, {dataStore});
      const putDataTracker = wrapPutData(scenario.sdk.dataManager);
      const bucketingEvents = countEventFires(
        scenario.sdk.eventManager,
        SystemEvents.BUCKETING
      );
      const conversionEvents = countEventFires(
        scenario.sdk.eventManager,
        SystemEvents.CONVERSION
      );
      const visitorId = 'visitor-ac5';
      const context = makeContext(scenario.sdk, visitorId);

      await context.setPreview({
        experienceId: scenario.previewExperienceId,
        variationId: scenario.targetVariationId
      });
      context.runExperience(scenario.previewExperienceKey);
      context.runExperience(scenario.normalExperienceKey, {
        ignoreLocationProperties: true
      });
      context.trackConversion(GOAL_KEY);

      await sleep(release_timeout + wait_margin);

      expect(trackHits, '/track requests').to.have.lengthOf(0);
      expect(dataStore.setCallCount, 'DataStore.set() calls').to.equal(0);
      expect(putDataTracker.calls, 'dataManager.putData() calls').to.equal(0);
      expect(scenario.sdk.dataManager.getData(visitorId), 'visitor store data').to
        .be.null;
      expect(
        (scenario.sdk.dataManager as any)._bucketedVisitors.size,
        'in-memory visitor store size'
      ).to.equal(0);
      expect(bucketingEvents.count, 'SystemEvents.BUCKETING fires').to.equal(0);
      expect(conversionEvents.count, 'SystemEvents.CONVERSION fires').to.equal(0);
    });
  });

  describe('AC6 -- isolation: preview state never leaks to a second context on the same SDK instance', function () {
    it('a second, non-preview context on the same SDK instance still buckets, persists, and tracks', async function () {
      this.timeout(test_timeout);
      const dataStore = new SpyDataStore();
      const scenario = buildScenario('ac6', previewResponses, {dataStore});
      const previewVisitorId = 'visitor-ac6-preview';
      const normalVisitorId = 'visitor-ac6-normal';
      const previewContext = makeContext(scenario.sdk, previewVisitorId);
      const normalContext = makeContext(scenario.sdk, normalVisitorId);

      await previewContext.setPreview({
        experienceId: scenario.previewExperienceId,
        variationId: scenario.targetVariationId
      });
      previewContext.runExperience(scenario.previewExperienceKey);

      const normalResult = normalContext.runExperience(scenario.normalExperienceKey, {
        ignoreLocationProperties: true
      });
      normalContext.trackConversion(GOAL_KEY);

      await sleep(release_timeout + wait_margin);

      expect(normalResult, 'normal context decision').to.be.an('object');
      expect(normalResult.experienceKey).to.equal(scenario.normalExperienceKey);
      expect(trackHits.length, '/track requests from the normal context').to.be
        .greaterThan(0);
      expect(dataStore.setCallCount, 'DataStore.set() calls from the normal context')
        .to.be.greaterThan(0);
      expect(
        scenario.sdk.dataManager.getData(normalVisitorId),
        'normal visitor store data'
      ).to.not.be.null;
      expect(
        scenario.sdk.dataManager.getData(previewVisitorId),
        'preview visitor store data'
      ).to.be.null;
    });
  });

  describe('AC7 -- inert on bad input: unknown experienceId (not resolvable via shared config or the ?exp= fetch)', function () {
    it('logs a WARNING and leaves subsequent runExperience/trackConversion fully normal', async function () {
      this.timeout(test_timeout);
      const dataStore = new SpyDataStore();
      // No previewResponses entry registered for this scenario's preview
      // experience id -- the shared server falls back to its default
      // `{experiences: []}` response, i.e. the ?exp= fetch does not resolve it
      // either. registerPreview: false documents that this is deliberate.
      const scenario = buildScenario('ac7', previewResponses, {
        dataStore,
        registerPreview: false
      });
      const visitorId = 'visitor-ac7';
      const context = makeContext(scenario.sdk, visitorId);

      await context.setPreview({
        experienceId: scenario.previewExperienceId,
        variationId: scenario.targetVariationId
      });

      expect(scenario.sdk.logger.warnings.length, 'logged WARNING count').to.be
        .greaterThan(0);

      const normalResult = context.runExperience(scenario.normalExperienceKey, {
        ignoreLocationProperties: true
      });
      context.trackConversion(GOAL_KEY);

      await sleep(release_timeout + wait_margin);

      expect(normalResult, 'normal decision').to.be.an('object');
      expect(normalResult.experienceKey).to.equal(scenario.normalExperienceKey);
      expect(trackHits.length, '/track requests').to.be.greaterThan(0);
      expect(dataStore.setCallCount, 'DataStore.set() calls').to.be.greaterThan(0);
    });
  });
});
