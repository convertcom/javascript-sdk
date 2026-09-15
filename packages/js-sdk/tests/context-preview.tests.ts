/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * qs-02 -- Context.setPreview() integration tests.
 *
 * Owns: AC3 (render half -- preview experience resolved only via the `?exp=` fetch),
 * AC5 (zero-trace, Node transport), AC6 (isolation across contexts on the same SDK
 * instance), AC7 (inert-on-bad-input, unknown-experience half).
 *
 * `enableStorage` (on `BucketingAttributes` / `DataManager`) is never set directly by
 * this file; it is exercised indirectly, end-to-end, through `Context.setPreview()`.
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
  ConfigLocation,
  ConfigSegment,
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

// Shared by makeLocation()/makeSegment() below -- both ConfigLocation and
// ConfigSegment are {id, key, name, rules} shaped, and both are matched via
// RuleManager.isRuleMatched() against a single `{[matchKey]: matchValue}`
// property. One rule-tree builder for both keeps the zero-trace STORAGE
// tests parameterizable and avoids copy-pasting the OR/AND/OR_WHEN tree.
function makeRuleMatchedEntity<T>(
  id: string,
  key: string,
  matchKey: string,
  matchValue: string
): T {
  return {
    id,
    key,
    name: `Entity ${key}`,
    rules: {
      OR: [
        {
          AND: [
            {
              OR_WHEN: [
                {
                  key: matchKey,
                  matching: {match_type: 'equals', negated: false},
                  value: matchValue
                }
              ]
            }
          ]
        }
      ]
    }
  } as unknown as T;
}

function makeLocation(
  id: string,
  key: string,
  matchKey: string,
  matchValue: string
): ConfigLocation {
  return makeRuleMatchedEntity<ConfigLocation>(id, key, matchKey, matchValue);
}

function makeSegment(
  id: string,
  key: string,
  matchKey: string,
  matchValue: string
): ConfigSegment {
  return makeRuleMatchedEntity<ConfigSegment>(id, key, matchKey, matchValue);
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

// Returns `any`: callers access `.experienceId`/`.id`/etc directly off
// `runExperience()`'s `BucketedVariation | RuleError | BucketingError` union
// without narrowing, matching this file's established assertion style (see
// `assertPreviewDecision`-style helpers elsewhere in this repo) -- not
// related to `setPreview()`, which is a fully-typed public method already.
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

// Zero-trace STORAGE (RED): runs `act()` against a freshly wrapped
// `putData` and asserts no store write happened at all, on any of the three
// observable surfaces this repo already uses for AC5 (dataManager.putData()
// call count, DataStore.set() call count, in-memory `_bucketedVisitors`
// size). Shared by every zero-trace STORAGE case below so each `it()` only
// supplies the scenario-specific `act` closure (SonarCloud 3% gate).
function expectZeroStorageWrites(
  sdk: Sdk,
  dataStore: SpyDataStore,
  act: () => void
): void {
  const putDataTracker = wrapPutData(sdk.dataManager);
  const bucketedVisitorsBefore = (sdk.dataManager as any)._bucketedVisitors
    .size;

  act();

  expect(putDataTracker.calls, 'dataManager.putData() calls').to.equal(0);
  expect(dataStore.setCallCount, 'DataStore.set() calls').to.equal(0);
  expect(
    (sdk.dataManager as any)._bucketedVisitors.size,
    'in-memory visitor store size'
  ).to.equal(bucketedVisitorsBefore);
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
  options: {
    dataStore?: SpyDataStore;
    registerPreview?: boolean;
    // Zero-trace STORAGE (RED): lets a scenario carry an extra, real,
    // NON-target experience (e.g. location-targeted) plus the top-level
    // `data.locations`/`data.segments` entities it resolves against,
    // without duplicating the rest of buildScenario() per test case.
    extraExperiences?: Array<ConfigExperience>;
    locations?: Array<ConfigLocation>;
    segments?: Array<ConfigSegment>;
  } = {}
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
      experiences: [normalExperience, ...(options.extraExperiences || [])],
      goals: [makeGoal()],
      ...(options.locations ? {locations: options.locations} : {}),
      ...(options.segments ? {segments: options.segments} : {})
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

// Shared by every test needing a location-targeted, NON-target (real,
// in-config) experience on top of a `buildScenario()` base -- the zero-trace
// STORAGE tests and the LOCATION event-suppression tests below all need the
// identical experience+location rule-matched fixture shape, differing only
// in match key/value, prefix, and their own `act`/assertion (SonarCloud 3%
// gate).
function buildLocationTargetedScenario(
  prefix: string,
  previewResponses: Map<string, Record<string, any>>,
  matchKey: string,
  matchValue: string,
  options: {dataStore?: SpyDataStore} = {}
): Scenario & {locationExperienceKey: string} {
  const locationExperienceKey = `${prefix}-exp-key`;
  const locationExperience = makeExperience(
    `${prefix}-exp`,
    locationExperienceKey,
    [`${prefix}-var-a`, `${prefix}-var-b`],
    {locations: [prefix]}
  );
  const scenario = buildScenario(prefix, previewResponses, {
    ...options,
    extraExperiences: [locationExperience],
    locations: [makeLocation(prefix, `${prefix}-key`, matchKey, matchValue)]
  });
  return {...scenario, locationExperienceKey};
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

  describe('run-all forcing -- runExperiences() forces the preview target too (parity with runExperience)', function () {
    it('forces the previewed (draft, ?exp=-only) experience into the runExperiences() result exactly once while other experiences decide normally, with zero trace', async function () {
      this.timeout(test_timeout);
      const dataStore = new SpyDataStore();
      const scenario = buildScenario('runall', previewResponses, {dataStore});
      const visitorId = 'visitor-runall';
      const context = makeContext(scenario.sdk, visitorId);

      await context.setPreview({
        experienceId: scenario.previewExperienceId,
        variationId: scenario.targetVariationId
      });
      const results = context.runExperiences({ignoreLocationProperties: true});

      // The previewed (draft, ?exp=-only) experience is forced into the run-all
      // result set with its requested variation -- exactly what
      // runExperience(key) returns -- and appears EXACTLY ONCE (never both a
      // normally-bucketed and a forced entry).
      const forcedEntries = results.filter(
        (r: any) => r.experienceKey === scenario.previewExperienceKey
      );
      expect(
        forcedEntries,
        'previewed experience entries in runExperiences()'
      ).to.have.lengthOf(1);
      expect(forcedEntries[0].experienceId).to.equal(scenario.previewExperienceId);
      expect(forcedEntries[0].id).to.equal(scenario.targetVariationId);

      // A non-previewed, in-config experience still decides normally in the
      // same call (contract §2 "other experiences still decide normally").
      const normalEntry = results.find(
        (r: any) => r.experienceKey === scenario.normalExperienceKey
      );
      expect(normalEntry, 'non-previewed experience still decided in run-all').to.be
        .an('object');

      // Zero-trace holds for run-all too (AC5): no /track, no store writes.
      await sleep(release_timeout + wait_margin);
      expect(trackHits, '/track requests').to.have.lengthOf(0);
      expect(dataStore.setCallCount, 'DataStore.set() calls').to.equal(0);
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

  // Zero-trace STORAGE regression locks: AC5's zero-trace guarantee must also
  // hold on the write paths that do NOT flow through
  // `ExperienceManager.selectVariation()`'s `enableStorage` forwarding -- the
  // location rule-matching path (`matchRulesByField()` -> `selectLocations()`,
  // which on a preview context operates on a defensive COPY of the visitor's
  // stored `locations` and gates `putData()` by `enableStorage`) and the
  // Context segment / visitor-property methods (`setDefaultSegments`,
  // `runCustomSegments`, `updateVisitorProperties`, each gated on
  // `this._preview`). Each case below asserts a preview context performs zero
  // store writes on one of those paths.
  describe('Zero-trace STORAGE: a preview context performs zero store writes on the location and segment/visitor-property paths', function () {
    const MATCH_KEY = 'country';
    const MATCH_VALUE = 'US';

    it('runExperience() on a location-targeted NON-target experience still writes to storage (matchRulesByField -> selectLocations -> putData, ungated by enableStorage)', async function () {
      this.timeout(test_timeout);
      const dataStore = new SpyDataStore();
      const scenario = buildLocationTargetedScenario(
        'zt-storage-loc',
        previewResponses,
        MATCH_KEY,
        MATCH_VALUE,
        {dataStore}
      );
      const context = makeContext(scenario.sdk, 'visitor-zt-storage-loc');

      await context.setPreview({
        experienceId: scenario.previewExperienceId,
        variationId: scenario.targetVariationId
      });

      expectZeroStorageWrites(scenario.sdk, dataStore, () =>
        context.runExperience(scenario.locationExperienceKey, {
          locationProperties: {[MATCH_KEY]: MATCH_VALUE}
        })
      );
    });

    // Zero-trace IN-PLACE MUTATION (RED): even with `putData()` now gated by
    // `enableStorage`, `selectLocations()` (data-manager.ts) destructures
    // `locations` straight off the object `getData()` returns and then
    // `push`/`splice`s it directly. On the memory-only path (no DataStore
    // configured, as in every scenario in this file) `getData()` returns the
    // SAME object reference stored in `_bucketedVisitors` -- so for a
    // visitorId that already has stored locations, this mutates that live,
    // shared array in place BEFORE `enableStorage` is ever consulted. A
    // preview context therefore corrupts a real visitor's stored data with
    // zero store-write trace. The case above only proves this on a FRESH
    // visitorId, where `getData()` returns `null` and the `{locations = []}`
    // destructure default creates a brand-new, unaliased array -- masking
    // this exact defect.
    it("runExperience() on a location-targeted NON-target experience mutates an ALREADY-STORED visitor's `locations` array in place, bypassing the `enableStorage` gate entirely", async function () {
      this.timeout(test_timeout);
      const scenario = buildLocationTargetedScenario(
        'zt-mutate-loc',
        previewResponses,
        MATCH_KEY,
        MATCH_VALUE
      );
      const visitorId = 'visitor-zt-mutate-loc';

      // Seed this visitor's REAL stored `locations` array on the shared
      // dataManager BEFORE any preview activity, then snapshot it (deep
      // copy) so mutation of the live array is unambiguously observable.
      scenario.sdk.dataManager.putData(visitorId, {locations: ['seed-other-loc']});
      const storedLocationsBeforePreview = JSON.parse(
        JSON.stringify(scenario.sdk.dataManager.getData(visitorId).locations)
      );

      const context = makeContext(scenario.sdk, visitorId);
      await context.setPreview({
        experienceId: scenario.previewExperienceId,
        variationId: scenario.targetVariationId
      });
      context.runExperience(scenario.locationExperienceKey, {
        locationProperties: {[MATCH_KEY]: MATCH_VALUE}
      });

      expect(
        scenario.sdk.dataManager.getData(visitorId).locations,
        "visitor's real stored `locations` array, read back after the preview run"
      ).to.deep.equal(storedLocationsBeforePreview);
    });

    it('runCustomSegments() on a preview context still writes matched custom segments to storage (no `_preview` guard)', async function () {
      this.timeout(test_timeout);
      const dataStore = new SpyDataStore();
      const segmentKey = 'zt-storage-seg-key';
      const scenario = buildScenario('zt-storage-seg', previewResponses, {
        dataStore,
        segments: [
          makeSegment('zt-storage-seg', segmentKey, MATCH_KEY, MATCH_VALUE)
        ]
      });
      const context = makeContext(scenario.sdk, 'visitor-zt-storage-seg');

      await context.setPreview({
        experienceId: scenario.previewExperienceId,
        variationId: scenario.targetVariationId
      });

      expectZeroStorageWrites(scenario.sdk, dataStore, () =>
        context.runCustomSegments([segmentKey], {
          ruleData: {[MATCH_KEY]: MATCH_VALUE}
        })
      );
    });

    // setDefaultSegments() and updateVisitorProperties() both write straight
    // to the store (segmentsManager.putSegments() / dataManager.putData())
    // with no `_preview` check anywhere on either call path -- parameterized
    // together since only the `act` closure differs between the two cases.
    [
      {
        slug: 'default-segments',
        name: 'setDefaultSegments()',
        act: (context: any, visitorId: string) =>
          context.setDefaultSegments({[MATCH_KEY]: MATCH_VALUE})
      },
      {
        slug: 'update-visitor-properties',
        name: 'updateVisitorProperties()',
        act: (context: any, visitorId: string) =>
          context.updateVisitorProperties(visitorId, {
            [MATCH_KEY]: MATCH_VALUE
          })
      }
    ].forEach(({slug, name, act}) => {
      it(`${name} on a preview context still writes to storage (no \`_preview\` guard)`, async function () {
        this.timeout(test_timeout);
        const dataStore = new SpyDataStore();
        const scenario = buildScenario(`zt-storage-${slug}`, previewResponses, {
          dataStore
        });
        const visitorId = `visitor-zt-storage-${slug}`;
        const context = makeContext(scenario.sdk, visitorId);

        await context.setPreview({
          experienceId: scenario.previewExperienceId,
          variationId: scenario.targetVariationId
        });

        expectZeroStorageWrites(scenario.sdk, dataStore, () =>
          act(context, visitorId)
        );
      });
    });
  });

  // FIX B: LOCATION_ACTIVATED/LOCATION_DEACTIVATED must be suppressed on a
  // preview context (zero-trace, AC5) -- but this suppression is
  // PREVIEW-scoped, NOT enableTracking-scoped: a normal silent run
  // (`enableTracking: false`) must still fire these events, exactly like
  // `SystemEvents.BUCKETING` already does today. Location/audience matching
  // itself is unaffected in every case below.
  describe('LOCATION event suppression is preview-scoped, not enableTracking-scoped', function () {
    const MATCH_KEY = 'country';
    const MATCH_VALUE = 'US';

    [
      {
        slug: 'preview-run',
        name: 'a preview context fires ZERO LOCATION_ACTIVATED events for a matching location-targeted, NON-target experience',
        preview: true,
        expectedFires: 0
      },
      {
        slug: 'normal-run',
        name: 'a normal run (no preview) still fires LOCATION_ACTIVATED for a matching location-targeted experience',
        preview: false,
        expectedFires: 1
      },
      {
        slug: 'silent-run',
        name: 'a normal run with enableTracking:false still fires LOCATION_ACTIVATED (regression -- suppression is NOT enableTracking-scoped)',
        preview: false,
        enableTracking: false,
        expectedFires: 1
      }
    ].forEach(({slug, name, preview, enableTracking, expectedFires}) => {
      it(name, async function () {
        this.timeout(test_timeout);
        const scenario = buildLocationTargetedScenario(
          `loc-evt-${slug}`,
          previewResponses,
          MATCH_KEY,
          MATCH_VALUE
        );
        const context = makeContext(scenario.sdk, `visitor-loc-evt-${slug}`);
        const activatedEvents = countEventFires(
          scenario.sdk.eventManager,
          SystemEvents.LOCATION_ACTIVATED
        );

        if (preview) {
          await context.setPreview({
            experienceId: scenario.previewExperienceId,
            variationId: scenario.targetVariationId
          });
        }

        context.runExperience(scenario.locationExperienceKey, {
          locationProperties: {[MATCH_KEY]: MATCH_VALUE},
          ...(enableTracking === false ? {enableTracking: false} : {})
        });

        expect(activatedEvents.count, 'LOCATION_ACTIVATED fires').to.equal(
          expectedFires
        );
      });
    });

    it('a preview context fires ZERO LOCATION_DEACTIVATED events when a previously-matched location stops matching', async function () {
      this.timeout(test_timeout);
      const scenario = buildLocationTargetedScenario(
        'loc-evt-deactivate',
        previewResponses,
        MATCH_KEY,
        MATCH_VALUE
      );
      const visitorId = 'visitor-loc-evt-deactivate';
      const context = makeContext(scenario.sdk, visitorId);

      // Match once (normally, before preview) so the location is stored --
      // required for a subsequent non-match to be a DEACTIVATION.
      context.runExperience(scenario.locationExperienceKey, {
        locationProperties: {[MATCH_KEY]: MATCH_VALUE}
      });

      await context.setPreview({
        experienceId: scenario.previewExperienceId,
        variationId: scenario.targetVariationId
      });
      const deactivatedEvents = countEventFires(
        scenario.sdk.eventManager,
        SystemEvents.LOCATION_DEACTIVATED
      );

      context.runExperience(scenario.locationExperienceKey, {
        locationProperties: {[MATCH_KEY]: 'non-matching-value'}
      });

      expect(deactivatedEvents.count, 'LOCATION_DEACTIVATED fires').to.equal(0);
    });
  });
});
