/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * CAP-3 (SPEC-per-call-bucketing-attributes) -- browser gate for the Node
 * guard in context-preview-feature-natural-bucketing.tests.ts: a caller
 * passing {enableTracking: true, enableStorage: true} to the feature entry
 * points on a previewing context must not defeat zero-trace on the real
 * transport, and the previewed experience's feature must still resolve via
 * the suppress-only bucketing path (never a forced short-circuit).
 *
 * Reuses the same fixture experience/feature already proven end-to-end in
 * umd-bundle.spec.ts (test-experience-ab-fullstack-2 / feature-1), so any
 * failure is attributable to preview behavior, not to fixture drift.
 */
import {test, expect} from '@playwright/test';
import type {Page} from '@playwright/test';
import {setup} from './page-helpers';

const PREVIEW_EXPERIENCE_KEY = 'test-experience-ab-fullstack-2';
const PREVIEW_EXPERIENCE_ID = '100218245';
const PREVIEW_VARIATION_ID = '100299456';
const CAP3_FEATURE_KEY = 'feature-1';

const RUN_PROPS = {
  locationProperties: {url: 'https://convert.com/'},
  visitorProperties: {varName3: 'something'}
};

interface PreviewLifecycleResult {
  trackCalls: Array<{transport: string; url: string}>;
  dataStoreSetCallCount: number;
  dataStoreKeys: string[];
}

/**
 * Runs in-page (via `page.addInitScript`, so it wraps the native APIs BEFORE
 * the UMD bundle -- and therefore before any SDK network call -- ever
 * executes). Records every `navigator.sendBeacon` / `window.fetch` call
 * whose target URL contains `/track`, tagged by transport, onto
 * `window.__trackCalls`.
 */
function installTrackSpies(): void {
  const w = window as any;
  w.__trackCalls = [] as Array<{transport: string; url: string}>;

  const originalSendBeacon = navigator.sendBeacon?.bind(navigator);
  if (originalSendBeacon) {
    navigator.sendBeacon = function (url: string, data?: any): boolean {
      if (typeof url === 'string' && url.includes('/track')) {
        w.__trackCalls.push({transport: 'sendBeacon', url});
      }
      return originalSendBeacon(url, data);
    };
  }

  const originalFetch = window.fetch?.bind(window);
  if (originalFetch) {
    window.fetch = function (input: any, init?: any): Promise<Response> {
      const url = typeof input === 'string' ? input : input?.url;
      if (typeof url === 'string' && url.includes('/track')) {
        w.__trackCalls.push({transport: 'fetch', url});
      }
      return originalFetch(input, init);
    };
  }
}

/**
 * Arms transport spies (before the UMD bundle loads), builds a real
 * DataStore + Context via the shared page-helpers factories, and calls
 * `Context.setPreview()` -- exposing the context on `window.__previewContext`
 * so later `page.evaluate()` calls in the test body can drive it directly.
 */
async function armPreviewLifecycle(
  page: Page,
  {experienceId, variationId}: {experienceId: string; variationId: string}
): Promise<void> {
  await page.addInitScript(installTrackSpies);
  await setup(page);
  await page.evaluate(
    async ({experienceId, variationId}) => {
      const w = window as any;
      const dataStore = w.__makeDataStore();
      const context = w.__createContext('XXX', undefined, {dataStore});
      w.__previewContext = context;
      w.__previewDataStore = dataStore;
      await context.setPreview({experienceId, variationId});
    },
    {experienceId, variationId}
  );
}

/**
 * Gives any batched/timer-based tracking (release_interval: 1000ms per
 * __createSdk()) a real chance to fire, then returns the fixed shape every
 * zero-trace assertion below reads.
 */
async function settlePreviewLifecycle(page: Page): Promise<PreviewLifecycleResult> {
  return page.evaluate(async () => {
    const w = window as any;
    await new Promise((resolve) => setTimeout(resolve, 1500));
    return {
      trackCalls: w.__trackCalls as Array<{transport: string; url: string}>,
      dataStoreSetCallCount: w.__previewDataStore.setCallCount as number,
      dataStoreKeys: Object.keys(w.__previewDataStore.data)
    };
  });
}

function assertZeroTrace(result: PreviewLifecycleResult): void {
  expect(result.trackCalls).toEqual([]);
  expect(
    result.trackCalls.filter((call) => call.transport === 'sendBeacon')
  ).toEqual([]);
  expect(
    result.trackCalls.filter((call) => call.transport === 'fetch')
  ).toEqual([]);
  expect(result.dataStoreSetCallCount).toBe(0);
  expect(result.dataStoreKeys).toEqual([]);
}

interface Cap3GuardResult {
  forcedEntryAttributes?: Record<string, any>;
}

/**
 * Spies `DataManager.getBucketing()` around one call to `method`, made with
 * the caller override, then restores it before returning. Proves the
 * previewed experience's feature is still bucketed through the
 * suppress-only path (never `getPreviewDecision`'s forced short-circuit).
 */
async function callFeatureEntryPointWithOverride(
  page: Page,
  {
    method,
    featureKey,
    previewExperienceKey,
    runProps
  }: {
    method: 'runFeature' | 'runFeatures';
    featureKey: string;
    previewExperienceKey: string;
    runProps: Record<string, any>;
  }
): Promise<Cap3GuardResult> {
  return page.evaluate(
    ({method, featureKey, previewExperienceKey, runProps}) => {
      const context = (window as any).__previewContext;
      const dataManager = (context as any)._dataManager;
      const original = dataManager.getBucketing.bind(dataManager);
      const calls: Array<{identity: string; attributes: any}> = [];
      dataManager.getBucketing = (
        visitorId: string,
        identity: string,
        attributes: any
      ) => {
        calls.push({identity, attributes});
        return original(visitorId, identity, attributes);
      };
      const overrides = {...runProps, enableTracking: true, enableStorage: true};
      if (method === 'runFeature') {
        context.runFeature(featureKey, overrides);
      } else {
        context.runFeatures(overrides);
      }
      dataManager.getBucketing = original;
      const forcedEntry = calls.find((call) => call.identity === previewExperienceKey);
      return {forcedEntryAttributes: forcedEntry?.attributes};
    },
    {method, featureKey, previewExperienceKey, runProps}
  );
}

test.describe('CAP-3 (SPEC-per-call-bucketing-attributes): feature entry points stay suppress-only against a caller override', () => {
  (['runFeature', 'runFeatures'] as const).forEach((method) => {
    test(`${method} produces ZERO /track requests and ZERO store writes when the caller passes {enableTracking: true, enableStorage: true} on a previewing context`, async ({
      page
    }) => {
      await armPreviewLifecycle(page, {
        experienceId: PREVIEW_EXPERIENCE_ID,
        variationId: PREVIEW_VARIATION_ID
      });

      const guard = await callFeatureEntryPointWithOverride(page, {
        method,
        featureKey: CAP3_FEATURE_KEY,
        previewExperienceKey: PREVIEW_EXPERIENCE_KEY,
        runProps: RUN_PROPS
      });

      // The previewed experience's feature is still bucketed via the
      // suppress-only path, not `getPreviewDecision`'s forced short-circuit.
      expect(guard.forcedEntryAttributes).toBeTruthy();
      expect(guard.forcedEntryAttributes).toMatchObject({
        enableTracking: false,
        enableStorage: false,
        suppressEvents: true
      });

      const result = await settlePreviewLifecycle(page);
      assertZeroTrace(result);
    });
  });
});
