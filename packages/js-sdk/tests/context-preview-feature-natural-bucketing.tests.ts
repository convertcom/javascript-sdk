/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * CAP-3 (SPEC-per-call-bucketing-attributes) -- Node guard: on a previewing
 * `Context`, `runFeature()`/`runFeatures()` must resolve the previewed
 * experience's feature via NATURAL (suppress-only) bucketing, never via
 * `getPreviewDecision()`'s forced short-circuit, and a caller's
 * enableTracking/enableStorage overrides must never escape suppression.
 *
 * Self-contained: its fixture experience is loaded directly into the SDK's
 * shared config, so `Context.setPreview()` resolves it via
 * `DataManager.getEntityById()` with no `?exp=` network fetch -- no
 * `http.createServer()` needed in this file.
 */
import 'mocha';
import {expect} from 'chai';

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
import {FeatureStatus, VariationChangeType} from '@convertcom/js-sdk-enums';
import {
  Config as ConfigType,
  ConfigExperience,
  ExperienceVariationConfig,
  ExperienceStatuses,
  VariationStatuses
} from '@convertcom/js-sdk-types';

const host = 'http://localhost';
const port = 8098;
const release_timeout = 300;
const wait_margin = 400;
const test_timeout = release_timeout + wait_margin + 3000;
const batch_size = 5;

const ACCOUNT_ID = 'preview-feature-guard-account';
const PROJECT_ID = 'preview-feature-guard-project';
const GOAL_ID = 'preview-feature-guard-goal-id';
const GOAL_KEY = 'preview-feature-guard-goal-key';

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
  return {id: GOAL_ID, key: GOAL_KEY, name: 'Preview Feature Guard Goal'};
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
    }
  };
}

let sdkCounter = 0;
let featureIdCounter = 0;

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
    sdkKey: `preview-feature-guard-sdk-key-${sdkCounter}`,
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

// CAP-3 (SPEC-per-call-bucketing-attributes): an IN-CONFIG experience
// carrying a `fullStackFeature` change, so `FeatureManager.runFeatures()`
// (which never special-cases `_preview`) can naturally bucket it while
// it is being previewed.
function buildFeatureScenario(
  prefix: string,
  options: {dataStore?: SpyDataStore} = {}
): {
  sdk: Sdk;
  previewExperienceId: string;
  previewExperienceKey: string;
  targetVariationId: string;
  otherVariationId: string;
  featureKey: string;
  variationMarkers: {target: string; other: string};
} {
  const previewExperienceId = `${prefix}-preview-exp`;
  const previewExperienceKey = `${prefix}-preview-exp-key`;
  const targetVariationId = `${prefix}-preview-var-target`;
  const otherVariationId = `${prefix}-preview-var-other`;
  featureIdCounter += 1;
  const featureId = 9000000 + featureIdCounter;
  const featureKey = `${prefix}-feature-key`;
  // CAP-3 (SPEC-per-call-bucketing-attributes): distinct variables_data plus
  // a zero-traffic target arm make the naturally-bucketed variation and the
  // forced preview target observably different.
  const variationMarkers = {target: `${prefix}-target-marker`, other: `${prefix}-other-marker`};

  const previewExperience = makeExperience(previewExperienceId, previewExperienceKey, [
    targetVariationId,
    otherVariationId
  ]);
  previewExperience.variations = previewExperience.variations.map((variation) => {
    const isTarget = variation.id === targetVariationId;
    return {
      ...variation,
      traffic_allocation: isTarget ? 0 : 100,
      changes: [
        {
          type: VariationChangeType.FULLSTACK_FEATURE,
          data: {
            feature_id: featureId,
            variables_data: {
              marker: isTarget ? variationMarkers.target : variationMarkers.other
            }
          }
        }
      ]
    } as unknown as ExperienceVariationConfig;
  });

  const sdk = makeSdk(
    {
      account_id: ACCOUNT_ID,
      project: {id: PROJECT_ID},
      experiences: [previewExperience],
      features: [{id: featureId, key: featureKey, name: 'CAP-3 guard feature'}],
      goals: [makeGoal()]
    },
    options
  );

  return {
    sdk,
    previewExperienceId,
    previewExperienceKey,
    targetVariationId,
    otherVariationId,
    featureKey,
    variationMarkers
  };
}

// Records `ApiManager.enqueue()` calls -- the step every `/track` POST is
// built from -- so this guard proves zero trace without needing an HTTP
// server or a batching-interval sleep.
function wrapApiManagerEnqueue(apiManager: InstanceType<typeof am>): {calls: number} {
  const tracker = {calls: 0};
  const original = apiManager.enqueue.bind(apiManager);
  (apiManager as any).enqueue = (...args: Parameters<typeof apiManager.enqueue>) => {
    tracker.calls++;
    return (original as any)(...args);
  };
  return tracker;
}

// Records `DataManager.getBucketing()` calls. `getPreviewDecision()` never
// calls it, so a captured call for `identity` proves natural bucketing
// rather than a forced short-circuit.
function spyGetBucketingCalls(
  dataManager: InstanceType<typeof dm>
): {calls: Array<{identity: string; attributes: any}>; restore: () => void} {
  const original = dataManager.getBucketing.bind(dataManager);
  const calls: Array<{identity: string; attributes: any}> = [];
  (dataManager as any).getBucketing = (
    visitorId: string,
    identity: string,
    attributes: any
  ) => {
    calls.push({identity, attributes});
    return (original as any)(visitorId, identity, attributes);
  };
  return {
    calls,
    restore: () => {
      (dataManager as any).getBucketing = original;
    }
  };
}

// CAP-3 (SPEC-per-call-bucketing-attributes): guards that CAP-1's fix
// never lets a caller's enableTracking/enableStorage escape a
// previewing context on the feature entry points.
describe('CAP-3 -- feature-path zero-trace guard: caller enableTracking/enableStorage never escape a previewing context', function () {
  [
    {
      name: 'runFeature',
      call: (context: any, featureKey: string) =>
        context.runFeature(featureKey, {
          enableTracking: true,
          enableStorage: true,
          ignoreLocationProperties: true
        })
    },
    {
      name: 'runFeatures',
      call: (context: any, _featureKey: string) =>
        context.runFeatures({
          enableTracking: true,
          enableStorage: true,
          ignoreLocationProperties: true
        })
    }
  ].forEach(({name, call}) => {
    it(`${name} stays suppress-only and resolves the previewed experience's feature from NATURAL bucketing, never a forced decision`, async function () {
      this.timeout(test_timeout);
      const dataStore = new SpyDataStore();
      const scenario = buildFeatureScenario(`cap3-${name}`, {dataStore});
      const context = makeContext(scenario.sdk, `visitor-cap3-${name}`);
      const bucketingSpy = spyGetBucketingCalls(scenario.sdk.dataManager);
      const enqueueTracker = wrapApiManagerEnqueue(scenario.sdk.apiManager);

      await context.setPreview({
        experienceId: scenario.previewExperienceId,
        variationId: scenario.targetVariationId
      });
      const featureResult = call(context, scenario.featureKey);

      const forcedEntry = bucketingSpy.calls.find(
        (entry) => entry.identity === scenario.previewExperienceKey
      );
      expect(
        forcedEntry,
        'DataManager.getBucketing() call for the previewed experience'
      ).to.exist;
      expect(forcedEntry.attributes).to.include({
        enableTracking: false,
        enableStorage: false,
        suppressEvents: true
      });
      bucketingSpy.restore();

      const entries = Array.isArray(featureResult) ? featureResult : [featureResult];
      const bucketedFeature: any = entries.find(
        (entry: any) => entry?.key === scenario.featureKey
      );
      expect(bucketedFeature, 'bucketed feature for the previewed experience').to
        .exist;
      expect(bucketedFeature.status).to.equal(FeatureStatus.ENABLED);
      // The target variation carries zero traffic allocation, so this can
      // only be populated by the naturally-bucketed OTHER variation --
      // never by `getPreviewDecision`'s forced target.
      expect(bucketedFeature.variables).to.deep.equal({
        marker: scenario.variationMarkers.other
      });
      expect(bucketedFeature.variables).to.not.deep.equal({
        marker: scenario.variationMarkers.target
      });

      expect(enqueueTracker.calls, 'ApiManager.enqueue() calls').to.equal(0);
      expect(dataStore.setCallCount, 'DataStore.set() calls').to.equal(0);
    });
  });
});
