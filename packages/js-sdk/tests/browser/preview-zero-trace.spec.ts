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
 * RED phase: `context.setPreview` does not exist yet. The in-page `page.evaluate()`
 * call throws (TypeError: context.setPreview is not a function), which Playwright
 * surfaces as a rejected promise -- the test fails for that reason. This is the
 * expected RED signal; do not attempt to make this test pass in this phase.
 *
 * Uses the same experiences/goal already proven to work end-to-end against the real
 * UMD bundle in umd-bundle.spec.ts's "Basic SDK methods" suite (same test-config.json,
 * same locationProperties/visitorProperties shape), so any failure once implemented is
 * attributable to preview behavior, not to unrelated rule-matching fixture drift.
 */
import {test, expect} from '@playwright/test';
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

test.describe('Context.setPreview() zero-trace on the real browser transport (RED)', () => {
  test('sends ZERO /track requests via sendBeacon or fetch across the full preview-context lifecycle', async ({
    page
  }) => {
    await page.addInitScript(installTrackSpies);
    await setup(page);

    const result = await page.evaluate(
      async ({previewExperienceId, previewVariationId, previewKey, otherKey, goalKey, runProps}) => {
        const w = window as any;
        const context = w.__defaultContext();

        await context.setPreview({
          experienceId: previewExperienceId,
          variationId: previewVariationId
        });

        const previewDecision = context.runExperience(previewKey, runProps);
        const otherDecision = context.runExperience(otherKey, runProps);
        context.trackConversion(goalKey, {ruleData: {action: 'buy'}});

        // Give any batched/timer-based tracking (release_interval: 1000ms per
        // __createSdk()) a real chance to fire before we assert on zero.
        await new Promise((resolve) => setTimeout(resolve, 1500));

        return {
          previewVariationId: previewDecision?.id,
          otherExperienceKey: otherDecision?.experienceKey,
          trackCalls: w.__trackCalls as Array<{transport: string; url: string}>
        };
      },
      {
        previewExperienceId: PREVIEW_EXPERIENCE_ID,
        previewVariationId: PREVIEW_VARIATION_ID,
        previewKey: PREVIEW_EXPERIENCE_KEY,
        otherKey: OTHER_EXPERIENCE_KEY,
        goalKey: GOAL_KEY,
        runProps: RUN_PROPS
      }
    );

    expect(result.trackCalls).toEqual([]);
    expect(
      result.trackCalls.filter((call) => call.transport === 'sendBeacon')
    ).toEqual([]);
    expect(
      result.trackCalls.filter((call) => call.transport === 'fetch')
    ).toEqual([]);
  });
});
