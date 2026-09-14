/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * qs-02 (SDK preview) -- AC5 zero-trace, verified against the REAL browser transport
 * (navigator.sendBeacon / window.fetch), not a Node-side http.createServer mock.
 *
 * This is the browser gate for AC5: across a full preview-context lifecycle --
 * `Context.setPreview()`, running the preview-target experience, running ANOTHER
 * (normal) experience, and attempting a conversion -- ZERO requests must reach the
 * `/track` endpoint via EITHER transport the SDK's HttpClient can choose
 * (packages/utils/src/http-client.ts: sendBeacon for POST when available, else fetch).
 *
 * Uses the same experiences/goal already proven to work end-to-end against the real
 * UMD bundle in umd-bundle.spec.ts's "Basic SDK methods" suite (same test-config.json,
 * same locationProperties/visitorProperties shape), so any failure is attributable to
 * preview behavior, not to unrelated rule-matching fixture drift.
 */
import {test, expect, Page} from '@playwright/test';
import {setup} from './page-helpers';

const PREVIEW_EXPERIENCE_KEY = 'test-experience-ab-fullstack-2';
const PREVIEW_EXPERIENCE_ID = '100218245';
const PREVIEW_VARIATION_ID = '100299456';
const OTHER_EXPERIENCE_KEY = 'test-experience-ab-fullstack-3';
const GOAL_KEY = 'increase-engagement';

// Same qualifying attributes umd-bundle.spec.ts already proved make these two
// experiences resolve deterministically (site_area url match + audience match).
const RUN_PROPS = {
  locationProperties: {url: 'https://convert.com/'},
  visitorProperties: {varName3: 'something'}
};

/**
 * Runs in-page (via `page.addInitScript`, so it wraps the native APIs BEFORE the
 * UMD bundle -- and therefore before any SDK network call -- ever executes).
 * Records every `navigator.sendBeacon` / `window.fetch` call whose target URL
 * contains `/track`, tagged by which transport carried it, onto
 * `window.__trackCalls` for later retrieval via `page.evaluate()`.
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

interface PreviewLifecycleResult {
  trackCalls: Array<{transport: string; url: string}>;
  dataStoreSetCallCount: number;
  dataStoreKeys: string[];
}

/**
 * Arms transport spies (before the UMD bundle loads), builds a real DataStore
 * + Context via the shared page-helpers factories, and calls
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
      // Real DataStore (see context-preview.tests.ts's zero-trace STORAGE
      // suite) with NO initial visitorProps -- unlike __defaultContext(),
      // whose {browser: 'chrome'} would persist as a segment write and
      // pollute this assertion.
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
  // Zero-trace STORAGE (browser hard gate, augmentation): no DataStore
  // write and no visitor-store growth across the entire lifecycle.
  expect(result.dataStoreSetCallCount).toBe(0);
  expect(result.dataStoreKeys).toEqual([]);
}

test.describe('Context.setPreview() zero-trace on the real browser transport', () => {
  test('sends ZERO /track requests via sendBeacon or fetch across the full preview-context lifecycle', async ({
    page
  }) => {
    await armPreviewLifecycle(page, {
      experienceId: PREVIEW_EXPERIENCE_ID,
      variationId: PREVIEW_VARIATION_ID
    });

    await page.evaluate(
      ({previewKey, otherKey, goalKey, runProps}) => {
        const context = (window as any).__previewContext;
        context.runExperience(previewKey, runProps);
        context.runExperience(otherKey, runProps);
        context.trackConversion(goalKey, {ruleData: {action: 'buy'}});
      },
      {
        previewKey: PREVIEW_EXPERIENCE_KEY,
        otherKey: OTHER_EXPERIENCE_KEY,
        goalKey: GOAL_KEY,
        runProps: RUN_PROPS
      }
    );

    const result = await settlePreviewLifecycle(page);
    assertZeroTrace(result);
  });
});
