/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * qs-01 (GOLD-1) — cross-SDK golden-vector bucketing parity, verified inside a real
 * headless Chromium browser against the actual built UMD bundle (not Node/Mocha).
 *
 * Spec of record: _bmad-output/planning-artifacts/2026-07-02-convert-js-sdk/qs-01-anchored-bucketing-layout.md
 * "Golden-vector fixture" section, AC6/AC7.
 *
 * This drives every vector in the SAME canonical fixture consumed by the Node/Mocha runner
 * (packages/data/tests/cross-sdk-vectors.tests.ts) — read directly from
 * packages/bucketing/tests/cross-sdk-bucketing-vectors.json, never duplicated — through the
 * real public SDK surface exposed on the UMD bundle's `window.ConvertSDK` global:
 * `new ConvertSDK.default({data}).createContext(visitorId, {}).runExperience(key, attrs)`.
 * That is the exact same production call chain the Node runner exercises:
 * `Context.runExperience` -> `ExperienceManager.selectVariation` ->
 * `DataManager.getBucketing` -> `DataManager._retrieveBucketing` (the anchored-vs-packed
 * GATE, `Number(experience.version) > 11`) -> `BucketingManager.getBucketForVisitor(Anchored)`.
 * No bucketing/gate logic is re-implemented here — only the fixture's per-vector experience
 * shape is assembled in-page and handed to the real bundled SDK.
 */
import {test, expect, Page} from '@playwright/test';
import * as fs from 'fs';
import * as path from 'path';

interface CrossSdkVariation {
  id: string;
  traffic_allocation?: number;
  status?: 'running' | 'stopped';
}

interface CrossSdkVector {
  description: string;
  experienceId: string;
  visitorId: string;
  version: number;
  variations: CrossSdkVariation[];
  expected: string | null;
}

interface VectorRunResult {
  description: string;
  expected: string | null;
  actualId: string | null;
  isVariationNotDecided: boolean;
}

// Same fixture the Node/Mocha runner reads (packages/data/tests/cross-sdk-vectors.tests.ts) --
// resolved via a relative filesystem path so no new package dependency edge is introduced.
const FIXTURE_PATH = path.resolve(
  __dirname,
  '../../../bucketing/tests/cross-sdk-bucketing-vectors.json'
);
const VECTORS: CrossSdkVector[] = JSON.parse(
  fs.readFileSync(FIXTURE_PATH, 'utf8')
);

// Drives every vector through the real UMD-bundled SDK in a single page context: one
// navigation, one page.evaluate() round-trip constructing a fresh ConvertSDK instance (direct
// `data` config, no network) per vector and calling the real public Context.runExperience()
// seam, then returns the per-vector outcome for assertion on the Node side.
async function runVectorsInBrowser(
  page: Page,
  vectors: CrossSdkVector[]
): Promise<VectorRunResult[]> {
  await page.goto('/umd.html');
  await page.waitForFunction(
    () => typeof (window as any).ConvertSDK?.default === 'function'
  );
  return page.evaluate((vecs: CrossSdkVector[]) => {
    const w = window as any;
    return vecs.map((vector) => {
      const config = {
        data: {
          account_id: 'browser-golden-vectors-account',
          project: {id: 'browser-golden-vectors-project'},
          experiences: [
            {
              id: vector.experienceId,
              name: `exp-${vector.experienceId}`,
              key: vector.experienceId,
              type: 'a/b_fullstack',
              audiences: [],
              goals: [],
              variations: vector.variations,
              version: vector.version
            }
          ]
        }
      };
      const sdk = new w.ConvertSDK.default(config);
      const context = sdk.createContext(vector.visitorId, {});
      const result = context.runExperience(vector.experienceId, {
        visitorProperties: {},
        ignoreLocationProperties: true,
        enableTracking: false,
        updateVisitorProperties: false
      });
      return {
        description: vector.description,
        expected: vector.expected,
        actualId:
          result && typeof result === 'object' && 'id' in result
            ? (result as {id: string}).id
            : null,
        isVariationNotDecided:
          result === w.ConvertSDK.BucketingError.VARIAION_NOT_DECIDED
      };
    });
  }, vectors);
}

test.describe('Cross-SDK golden-vector bucketing parity (qs-01 / GOLD-1) — real UMD bundle in headless Chromium', () => {
  test('all golden vectors resolve identically through the real bundled DataManager gate (packed v11 + anchored v12)', async ({
    page
  }) => {
    const results = await runVectorsInBrowser(page, VECTORS);
    expect(results).toHaveLength(VECTORS.length);
    for (const result of results) {
      if (result.expected === null) {
        expect(result.isVariationNotDecided, result.description).toBe(true);
      } else {
        expect(result.actualId, result.description).toBe(result.expected);
      }
    }
  });

  test('loaded every required golden-vector category, matching the Node/Mocha runner completeness guard (AC7)', () => {
    const descriptions = VECTORS.map((vector) => vector.description).join(
      '\n'
    );
    [
      '[packed-regression]',
      '[anchored-basic-thirds]',
      '[per-sliver-admission]',
      '[anchored-idle]',
      '[stopped-arm-stability]',
      '[ta-zero-width]',
      '[nan-default]',
      '[single-arm-v11-eq-v12]',
      '[100pct-total-v11-eq-v12]',
      '[boundary-hit]',
      '[total-weight-zero]'
    ].forEach((tag) => {
      expect(descriptions).toContain(tag);
    });
    expect(VECTORS.length).toBeGreaterThan(0);
  });
});
