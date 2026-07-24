/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * SDK-3 (RED) -- qs-03 mutual-exclusion audience rule (`bucketed_into_experience_key`),
 * verified inside a real headless Chromium browser against the actual built UMD bundle
 * (not Node/Mocha) -- the browser gate mandated by qs-03's "Mandated tests" section
 * ("the tracking script vendors these rule packages, so browser-engine equivalence
 * must be proven even though the TS does not serve this rule type by default").
 *
 * Spec of record: _bmad-output/planning-artifacts/2026-07-02-convert-js-sdk/qs-03-mutual-exclusion-rule.md
 * "Inline cross-SDK fixture" (AC1) + "Mandated tests" (browser gate, MUST).
 *
 * Reads the SAME 8-row fixture the Node/Mocha unit suite reads
 * (packages/data/tests/mutual-exclusion-rule-fixture.json) directly off disk -- never
 * duplicated -- and drives every row through the real public SDK surface exposed on
 * the UMD bundle's `window.ConvertSDK` global, mirroring golden-vectors.spec.ts's
 * pattern (fresh `ConvertSDK.default({data})` per case, real `Context.runExperience()`).
 *
 * Each row attaches its `bucketed_into_experience_key` rule to a TRANSIENT audience on
 * `exp-b` (the same "experience under test" convention used by the Node unit suite and
 * the js-sdk integration suite), so "served" (non-null) iff the negated-exclusion
 * audience matches. Stored bucketing is seeded either by reaching into the Context's
 * `_dataManager` (a runtime-accessible instance property -- TypeScript `private` is
 * compile-time only, and the built UMD bundle carries no privacy at all) to call the
 * exact same `putData`/`dataStoreManager.set` seams the Node unit suite uses directly,
 * NOT by re-running experiences to bucket -- avoids a circular seeding dependency for
 * row 5 (which seeds exp-b's OWN bucketing entry, the very experience hosting the rule
 * under test). See this feature's decision log for the full reasoning.
 *
 * RED-phase note: this spec is authored only in the RED phase -- it is NOT run here (it
 * requires the built UMD bundle AND a non-sandboxed browser launch). The conductor runs
 * it in the GREEN/full-suite phase, once the resolution seam lands in
 * `packages/data/src/data-manager.ts` and the UMD bundle is rebuilt.
 */
import {test, expect, Page} from '@playwright/test';
import * as fs from 'fs';
import * as path from 'path';

interface FixtureRow {
  row: number;
  description: string;
  storedBucketing: Record<string, string>;
  dataStoreOnly: boolean;
  ruleValue: string;
  negated: boolean;
  expectedMatched: boolean;
  expectWarn: boolean;
}

interface Fixture {
  config: {experiences: Array<Record<string, any>>};
  ruleDefaults: {rule_type: string; matching: {match_type: string}};
  rows: FixtureRow[];
}

// Same fixture the Node/Mocha unit runner reads
// (packages/data/tests/mutual-exclusion-rule.tests.ts) -- resolved via a relative
// filesystem path so no new package dependency edge is introduced.
const FIXTURE_PATH = path.resolve(
  __dirname,
  '../../../data/tests/mutual-exclusion-rule-fixture.json'
);
const FIXTURE: Fixture = JSON.parse(fs.readFileSync(FIXTURE_PATH, 'utf8'));

const EXPERIENCE_UNDER_TEST_KEY = 'exp-b';
const AUDIENCE_ID = 'qs-03-exclusion-audience';

interface RowRunResult {
  row: number;
  description: string;
  expectedMatched: boolean;
  matched: boolean;
}

// Drives every fixture row through the real UMD-bundled SDK in a single page context:
// one navigation, one page.evaluate() round-trip building a fresh ConvertSDK instance
// per row (direct `data` config, no network) and calling the real public
// `Context.runExperience()` seam -- no rule-matching logic is re-implemented here, only
// the fixture's per-row config/seeding is assembled in-page and handed to the real
// bundled SDK.
async function runRowsInBrowser(
  page: Page,
  fixture: Fixture
): Promise<RowRunResult[]> {
  await page.goto('/umd.html');
  await page.waitForFunction(
    () => typeof (window as any).ConvertSDK?.default === 'function'
  );
  return page.evaluate(
    ({rows, experiencesTemplate, ruleDefaults, audienceId, experienceUnderTestKey}) => {
      const w = window as any;
      return rows.map((row: FixtureRow) => {
        const visitorId = `browser-visitor-row-${row.row}`;
        const audience = {
          id: audienceId,
          key: audienceId,
          type: 'transient',
          status: 'active',
          rules: {
            OR: [
              {
                AND: [
                  {
                    OR_WHEN: [
                      {
                        ...ruleDefaults,
                        matching: {...ruleDefaults.matching, negated: row.negated},
                        value: row.ruleValue
                      }
                    ]
                  }
                ]
              }
            ]
          }
        };
        const experiences = experiencesTemplate.map((experience: Record<string, any>) =>
          experience.key === experienceUnderTestKey
            ? {
                ...experience,
                audiences: [audienceId],
                settings: {matching_options: {audiences: 'all'}}
              }
            : experience
        );
        const config = {
          data: {
            account_id: 'browser-qs-03-account',
            project: {id: 'browser-qs-03-project'},
            experiences,
            audiences: [audience]
          }
        };
        const sdk = new w.ConvertSDK.default(config);
        const context = sdk.createContext(visitorId, {});

        if (Object.keys(row.storedBucketing).length) {
          if (row.dataStoreOnly) {
            // Seed ONLY the DataStore, bypassing the in-memory visitor store entirely
            // -- proves `getData()` merges the DataStore in (row 8's contract).
            const dataStore = {
              data: {} as Record<string, any>,
              get(key: string) {
                return this.data[key];
              },
              set(key: string, value: any) {
                this.data[key] = value;
              }
            };
            context._dataManager.setDataStore(dataStore);
            const storeKey = context._dataManager.getStoreKey(visitorId);
            context._dataManager.dataStoreManager.set(storeKey, {
              bucketing: row.storedBucketing
            });
          } else {
            // In-memory seeding -- same seam the Node unit suite calls directly.
            context._dataManager.putData(visitorId, {bucketing: row.storedBucketing});
          }
        }

        const result = context.runExperience(experienceUnderTestKey, {
          visitorProperties: {}, // AC4
          ignoreLocationProperties: true
        });

        return {
          row: row.row,
          description: row.description,
          expectedMatched: row.expectedMatched,
          matched: Boolean(result)
        };
      });
    },
    {
      rows: fixture.rows,
      experiencesTemplate: fixture.config.experiences,
      ruleDefaults: fixture.ruleDefaults,
      audienceId: AUDIENCE_ID,
      experienceUnderTestKey: EXPERIENCE_UNDER_TEST_KEY
    }
  );
}

test.describe('Mutual-exclusion audience rule (qs-03 / SDK-3) -- real UMD bundle in headless Chromium', () => {
  test('all 8 fixture rows resolve identically through the real bundled DataManager/RuleManager (browser-engine equivalence, AC1)', async ({
    page
  }) => {
    const results = await runRowsInBrowser(page, FIXTURE);
    expect(results).toHaveLength(FIXTURE.rows.length);
    for (const result of results) {
      expect(result.matched, `row ${result.row}: ${result.description}`).toBe(
        result.expectedMatched
      );
    }
  });

  test('loaded the full 8-row cross-SDK fixture, matching the Node/Mocha unit runner (AC1 completeness guard)', () => {
    expect(FIXTURE.rows).toHaveLength(8);
    expect(FIXTURE.rows.filter((row) => row.expectedMatched)).toHaveLength(4);
    expect(FIXTURE.rows.filter((row) => row.expectWarn)).toHaveLength(2);
  });
});
