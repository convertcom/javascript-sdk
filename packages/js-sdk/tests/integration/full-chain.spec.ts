import {test, expect} from '@playwright/test';

// Import from the built CJS bundle — same as consumers would use
// eslint-disable-next-line @typescript-eslint/no-var-requires
const SDK = require('../../lib/index');
const ConvertSDK = SDK.default;
const {SystemEvents} = SDK;

// Static config for offline/always-run mode
// eslint-disable-next-line @typescript-eslint/no-var-requires
const staticConfig = require('./static-config.json');

// --- Constants matching staging project "FS-Test-Proj - DO NOT DELETE" ---
const EXPERIENCE_KEY = 'test-experience-ab-fullstack-4';
const FEATURE_TYPED_KEY = 'feature-2';
const FEATURE_BASIC_KEY = 'feature-1';
const GOAL_KEY = 'increase-engagement';
const VARIATION_IDS = ['1003180877', '1003180878'];

// Experience -1 has audience "adv-audience" requiring (desktop=true AND browser!="CH") OR (mobile=true)
const EXPERIENCE_WITH_AUDIENCE_KEY = 'test-experience-ab-fullstack-1';

// Location "pricing-location" requires location=pricing
const qualifyingAttributes = {
  locationProperties: {location: 'pricing'}
};

// --- In-memory DataStore for dedup tests ---
class MemoryDataStore {
  private data: Record<string, any> = {};
  get(key: string): any {
    if (!key) return this.data;
    return this.data[key.toString()];
  }
  set(key: string, value: any): void {
    if (!key) throw new Error('Invalid DataStore key!');
    this.data[key.toString()] = value;
  }
}

// --- Dual-mode: static always runs, live only when env var is set ---
const modes: Array<'static' | 'live' | 'live-secret'> = [
  'static',
  ...(process.env.CONVERT_STAGING_SDK_KEY ? (['live'] as const) : []),
  ...(process.env.CONVERT_STAGING_SDK_KEY2 &&
  process.env.CONVERT_STAGING_SDK_KEY2_SECRET
    ? (['live-secret'] as const)
    : [])
];

function createSdk(
  mode: 'static' | 'live' | 'live-secret',
  overrides: Record<string, any> = {}
) {
  if (mode === 'live') {
    return new ConvertSDK({
      sdkKey: process.env.CONVERT_STAGING_SDK_KEY,
      environment: 'staging',
      network: {tracking: false},
      ...overrides
    });
  }
  if (mode === 'live-secret') {
    return new ConvertSDK({
      sdkKey: process.env.CONVERT_STAGING_SDK_KEY2,
      sdkKeySecret: process.env.CONVERT_STAGING_SDK_KEY2_SECRET,
      environment: 'staging',
      network: {tracking: false},
      ...overrides
    });
  }
  return new ConvertSDK({
    data: staticConfig,
    environment: 'staging',
    network: {tracking: false},
    ...overrides
  });
}

// Helper: create SDK, wait for ready, and create a visitor context
async function createReadyContext(
  mode: 'static' | 'live' | 'live-secret',
  visitorId: string,
  overrides: Record<string, any> = {}
) {
  const sdk = createSdk(mode, overrides);
  await sdk.onReady();
  const context = sdk.createContext(visitorId);
  return {sdk, context};
}

// Helper: create SDK with tracking enabled and a fresh MemoryDataStore
function createTrackingSdk(
  mode: 'static' | 'live' | 'live-secret',
  overrides: Record<string, any> = {}
) {
  const dataStore = new MemoryDataStore();
  const sdk = createSdk(mode, {
    network: {tracking: true},
    dataStore,
    ...overrides
  });
  return {sdk, dataStore};
}

// Helper: create tracking SDK, wait for ready, create context, and bucket into experience
async function createBucketedTrackingContext(
  mode: 'static' | 'live' | 'live-secret',
  visitorId: string,
  overrides: Record<string, any> = {}
) {
  const {sdk, dataStore} = createTrackingSdk(mode, overrides);
  await sdk.onReady();
  const context = sdk.createContext(visitorId);
  context.runExperience(EXPERIENCE_KEY, qualifyingAttributes);
  return {sdk, dataStore, context};
}

// Helper: bucket into experience + run feature, return both with assertions
function bucketAndVerifyFeature(context: any) {
  const variation = context.runExperience(EXPERIENCE_KEY, qualifyingAttributes);
  expect(variation).toBeDefined();
  expect(VARIATION_IDS).toContain(variation.id);

  const feature = context.runFeature(FEATURE_TYPED_KEY, qualifyingAttributes);
  expect(feature).toBeDefined();
  expect(feature.status).toBe('enabled');
  expect(feature.variables.price).toBe(100);

  return {variation, feature};
}

for (const mode of modes) {
  test.describe(`Full-chain integration tests [${mode} mode]`, () => {
    // --- Happy Path ---

    test('SDK initializes and is ready', async () => {
      const sdk = createSdk(mode);
      await sdk.onReady();
    });

    test('Ready event fired on init', async () => {
      const sdk = createSdk(mode);
      let readyCalled = 0;
      let readyError: any = null;
      sdk.on(SystemEvents.READY, (_args: any, err: any) => {
        readyCalled++;
        readyError = err;
      });
      await sdk.onReady();
      expect(readyCalled).toBe(1);
      expect(readyError).toBeNull();
    });

    test('Create context and run experience', async () => {
      const {context} = await createReadyContext(
        mode,
        'visitor-integration-test'
      );
      const variation = context.runExperience(
        EXPERIENCE_KEY,
        qualifyingAttributes
      );
      expect(variation).toBeDefined();
      expect(variation).not.toBeNull();
      expect(variation.experienceKey).toBe(EXPERIENCE_KEY);
      expect(VARIATION_IDS).toContain(variation.id);
      expect(variation.changes).toBeDefined();
      expect(Array.isArray(variation.changes)).toBe(true);
      expect(variation.changes.length).toBeGreaterThan(0);
    });

    test('Bucketing determinism', async () => {
      const {context} = await createReadyContext(
        mode,
        'visitor-determinism-test'
      );

      const results: string[] = [];
      for (let i = 0; i < 10; i++) {
        const variation = context.runExperience(
          EXPERIENCE_KEY,
          qualifyingAttributes
        );
        expect(variation).toBeDefined();
        results.push(variation.id);
      }

      // All 10 runs should return the same variation
      const uniqueIds = [...new Set(results)];
      expect(uniqueIds).toHaveLength(1);
      expect(VARIATION_IDS).toContain(uniqueIds[0]);
    });

    test('Bucketing event fired on experience', async () => {
      const {sdk, context} = await createReadyContext(
        mode,
        'visitor-bucketing-event-test'
      );

      let bucketingFired = false;
      sdk.on(SystemEvents.BUCKETING, () => {
        bucketingFired = true;
      });
      context.runExperience(EXPERIENCE_KEY, qualifyingAttributes);
      expect(bucketingFired).toBe(true);
    });

    test('Run feature with typed variables', async () => {
      const {context} = await createReadyContext(
        mode,
        'visitor-feature-typed-test'
      );

      // Must run experience first to bucket into a variation
      context.runExperience(EXPERIENCE_KEY, qualifyingAttributes);

      const feature = context.runFeature(
        FEATURE_TYPED_KEY,
        qualifyingAttributes
      );
      expect(feature).toBeDefined();
      expect(feature).not.toBeNull();
      expect(feature.status).toBe('enabled');

      // Verify typed variables
      expect(feature.variables).toBeDefined();
      expect(typeof feature.variables.price).toBe('number');
      expect(feature.variables.price).toBe(100);
      expect(typeof feature.variables['button-height']).toBe('number');
      expect(feature.variables['button-height']).toBe(40);
      expect(typeof feature.variables.additionalData).toBe('object');
      expect(feature.variables.additionalData.foo).toBe('bar');
      expect(feature.variables.additionalData.v).toBe(2);
    });

    test('Full chain: init -> context -> bucket -> feature -> verify', async () => {
      const {sdk, context} = await createReadyContext(
        mode,
        'visitor-full-chain-test'
      );

      let readyFired = false;
      let bucketingFired = false;
      sdk.on(SystemEvents.READY, () => {
        readyFired = true;
      });
      sdk.on(SystemEvents.BUCKETING, () => {
        bucketingFired = true;
      });

      expect(readyFired).toBe(true);
      bucketAndVerifyFeature(context);
      expect(bucketingFired).toBe(true);
    });

    // --- Negative Path ---

    test('Run feature with unknown key returns disabled status', async () => {
      const {context} = await createReadyContext(
        mode,
        'visitor-negative-test'
      );
      const result = context.runFeature('nonexistent-feature');
      // SDK returns a BucketedFeature with status 'disabled' for unknown features
      expect(result).toBeDefined();
      expect(result.key).toBe('nonexistent-feature');
      expect(result.status).toBe('disabled');
    });

    test('Run experience with non-qualifying location returns null', async () => {
      const {context} = await createReadyContext(
        mode,
        'visitor-non-qualifying-test'
      );
      const result = context.runExperience(EXPERIENCE_KEY, {
        locationProperties: {location: 'nonexistent'}
      });
      // SDK returns null when location doesn't match
      expect(result).toBeNull();
    });

    test('Run experience with audience and no visitor properties returns null', async () => {
      const {context} = await createReadyContext(
        mode,
        'visitor-audience-test'
      );
      // exp-1 has audience requiring (desktop=true AND browser!="CH") OR (mobile=true)
      // Passing only locationProperties and no visitorProperties should fail audience check
      const result = context.runExperience(EXPERIENCE_WITH_AUDIENCE_KEY, {
        locationProperties: {location: 'pricing'}
      });
      // SDK returns null when audience doesn't match
      expect(result).toBeNull();
    });

    // --- Conversion Tracking ---

    test('Track conversion', async () => {
      const {context} = await createBucketedTrackingContext(
        mode,
        'visitor-conversion-test'
      );
      // increase-engagement goal has rules: null, so no ruleData should be passed
      const result = context.trackConversion(GOAL_KEY);
      // trackConversion returns undefined on success
      expect(result).toBeUndefined();
    });

    test('Conversion event fired on trackConversion', async () => {
      const {sdk, context} = await createBucketedTrackingContext(
        mode,
        'visitor-conversion-event-test'
      );

      let conversionFired = false;
      let conversionData: any = null;
      sdk.on(SystemEvents.CONVERSION, (data: any) => {
        conversionFired = true;
        conversionData = data;
      });

      // increase-engagement goal has rules: null, so no ruleData should be passed
      context.trackConversion(GOAL_KEY);

      expect(conversionFired).toBe(true);
      expect(conversionData).toBeDefined();
    });

    test('Goal deduplication', async () => {
      const {sdk, context} = await createBucketedTrackingContext(
        mode,
        'visitor-dedup-test'
      );

      let conversionCount = 0;
      sdk.on(SystemEvents.CONVERSION, () => {
        conversionCount++;
      });

      // First call — increase-engagement has no rules, so no ruleData
      context.trackConversion(GOAL_KEY);
      // Second call — should be deduplicated
      context.trackConversion(GOAL_KEY);

      expect(conversionCount).toBe(1);
    });

    test('Track conversion with revenue', async () => {
      const {sdk, context} = await createBucketedTrackingContext(
        mode,
        'visitor-revenue-test'
      );

      let queueReleased = false;
      sdk.on(SystemEvents.API_QUEUE_RELEASED, () => {
        queueReleased = true;
      });

      // increase-engagement goal has rules: null, so no ruleData
      const result = context.trackConversion(GOAL_KEY, {
        conversionData: [
          {key: 'amount', value: 49.99},
          {key: 'transactionId', value: 'txn-integration-001'}
        ]
      });

      expect(result).toBeUndefined();

      // Wait for the queue to release
      await new Promise<void>((resolve) => {
        if (queueReleased) {
          resolve();
        } else {
          const checkInterval = setInterval(() => {
            if (queueReleased) {
              clearInterval(checkInterval);
              resolve();
            }
          }, 100);
          // Timeout after 5 seconds
          setTimeout(() => {
            clearInterval(checkInterval);
            resolve();
          }, 5000);
        }
      });
    });

    test('Force multiple transactions', async () => {
      const {sdk, context} = await createBucketedTrackingContext(
        mode,
        'visitor-force-multi-test'
      );

      let conversionCount = 0;
      sdk.on(SystemEvents.CONVERSION, () => {
        conversionCount++;
      });

      // First call — no ruleData since increase-engagement has rules: null
      context.trackConversion(GOAL_KEY, {
        conversionData: [{key: 'amount', value: 10}]
      });

      // Second call with forceMultipleTransactions — should NOT be deduplicated
      context.trackConversion(GOAL_KEY, {
        conversionData: [{key: 'amount', value: 20}],
        conversionSetting: {forceMultipleTransactions: true}
      });

      expect(conversionCount).toBe(2);
    });

    test('Track conversion with nonexistent goal returns undefined', async () => {
      const {sdk, context} = await createBucketedTrackingContext(
        mode,
        'visitor-fake-goal-test'
      );

      let conversionFired = false;
      sdk.on(SystemEvents.CONVERSION, () => {
        conversionFired = true;
      });

      const result = context.trackConversion('totally-fake-goal');
      expect(result).toBeUndefined();
      expect(conversionFired).toBe(false);
    });

    // --- Complete Chain ---

    test('Complete chain: init -> context -> bucket -> feature -> conversion -> flush', async () => {
      const {sdk, context} = await createBucketedTrackingContext(
        mode,
        'visitor-complete-chain-test',
        {events: {batch_size: 1, release_interval: 1000}}
      );

      let conversionFired = false;
      sdk.on(SystemEvents.CONVERSION, () => {
        conversionFired = true;
      });

      // Verify feature (experience already bucketed by createBucketedTrackingContext)
      const feature = context.runFeature(
        FEATURE_TYPED_KEY,
        qualifyingAttributes
      );
      expect(feature).toBeDefined();
      expect(feature.status).toBe('enabled');
      expect(feature.variables.price).toBe(100);

      // Track conversion — no ruleData since increase-engagement has rules: null
      const conversionResult = context.trackConversion(GOAL_KEY, {
        conversionData: [{key: 'amount', value: 99.99}]
      });
      expect(conversionResult).toBeUndefined();
      expect(conversionFired).toBe(true);
    });
  });
}
