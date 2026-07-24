/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * SDK-3 (RED) -- qs-03 mutual-exclusion audience rule (`bucketed_into_experience_key`).
 *
 * Spec of record: _bmad-output/planning-artifacts/2026-07-02-convert-js-sdk/qs-03-mutual-exclusion-rule.md
 * "The contract (normative)" + "Inline cross-SDK fixture" + AC1/AC4/AC5/AC8.
 *
 * This is the JS SDK's own copy of the identical cross-SDK 8-row fixture
 * (packages/data/tests/mutual-exclusion-rule-fixture.json) -- every sibling SDK
 * (PHP/Python/Ruby/Android/iOS) consumes the same 8 rows verbatim; the expected
 * `matched` values are the normative contract, not a local test tweak.
 *
 * Driven through the REAL audience-evaluation path (`DataManager.matchRulesByField`,
 * which internally calls `filterMatchedRecordsWithRule` -> `RuleManager.isRuleMatched`)
 * -- never a private helper called directly -- so this test survives whichever exact
 * seam shape the GREEN phase lands (`filterMatchedRecordsWithRule` resolution per the
 * story's seam contract). `matchRulesByField`'s public signature (visitorId, identity,
 * identityField, attributes) is stable regardless of that internal choice.
 *
 * RED-phase note: the resolution seam does not exist yet in `data-manager.ts`. Today,
 * `RuleManager._processRuleItem` fail-closes on this unknown `rule_type` (no `key`
 * field, empty `visitorProperties` fails `objectNotEmpty`) and always returns `false`
 * with negation NOT applied on the fall-through (verified fact in the spec's "Verified
 * facts" section). That means rows expecting `matched: false` (1, 4, 6, 8) trivially
 * pass pre-seam -- this is expected, standard contract-table behavior: the fixture's
 * paired true/false rows (1 vs 2, 3 vs 4, negated toggles, stored-state toggles) jointly
 * constrain the implementation so that no degenerate function (always-true,
 * always-false) can satisfy the whole table. Rows 2, 3, 5, 7 (expecting `true`) and the
 * warn assertions on rows 6/7 are the rows that actually fail red today; see this
 * feature's decision log for the full reasoning.
 */
import 'mocha';
import {expect} from 'chai';
import {BucketingManager as bm} from '@convertcom/js-sdk-bucketing';
import {RuleManager as rm} from '@convertcom/js-sdk-rules';
import {EventManager as em} from '@convertcom/js-sdk-event';
import {ApiManager as am} from '@convertcom/js-sdk-api';
import {DataManager as dm} from '../src/data-manager';
import testConfig from './test-config.json';
import fixture from './mutual-exclusion-rule-fixture.json';
import {
  Config as ConfigType,
  ConfigExperience,
  ConfigAudienceTypes,
  GenericListMatchingOptions
} from '@convertcom/js-sdk-types';
import {objectDeepMerge} from '@convertcom/js-sdk-utils';
import {defaultConfig} from '../../js-sdk/src/config/default';

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

// Minimal DataStore double -- mirrors the `DataStore` class in data-manager.tests.ts,
// used only for row 8 (bucketing present ONLY in the DataStore, never in-memory).
class FixtureDataStore {
  data: Record<string, any> = {};
  get(key: string): any {
    if (!key) return this.data;
    return this.data[key.toString()];
  }
  set(key: string, value: any): void {
    if (!key) throw new Error('Invalid DataStore key!');
    this.data[key.toString()] = value;
  }
}

// Fake LogManager -- collects warn() calls for the AC8 assertion without pulling in a
// mocking library (mirrors data-manager-preview-decision.tests.ts's no-sinon convention:
// neither this package nor js-sdk-data declares sinon as a dependency).
class SpyLogger {
  warnCalls: any[][] = [];
  trace(): void {}
  debug(): void {}
  info(): void {}
  log(): void {}
  error(): void {}
  warn(...args: any[]): void {
    this.warnCalls.push(args);
  }
}

const ROWS = fixture.rows as FixtureRow[];
const ACCOUNT_ID = 'qs-03-account';
const PROJECT_ID = 'qs-03-project';
// The experience under test: it carries the exclusion audience naming the row's
// target (mirrors AC2's "experience B carries the exclusion audience").
const EXPERIENCE_UNDER_TEST_KEY = 'exp-b';
const AUDIENCE_ID = 'qs-03-exclusion-audience';

// Shared dependency managers (mirrors data-manager.tests.ts / cross-sdk-vectors.tests.ts
// pattern) -- stateless w.r.t. rule evaluation, no per-row setup/teardown needed for these.
const sharedConfiguration = objectDeepMerge(
  testConfig,
  defaultConfig,
  {}
) as unknown as ConfigType;
const bucketingManager = new bm(sharedConfiguration);
const ruleManager = new rm(sharedConfiguration);
const eventManager = new em(sharedConfiguration);
const apiManager = new am(sharedConfiguration, {eventManager});

/**
 * Builds the audience carrying the row's `bucketed_into_experience_key` rule, using the
 * fixture's own `ruleDefaults` (rule_type + match_type) merged with the row's
 * value/negated -- so no rule shape is duplicated by hand per row.
 */
function buildExclusionAudience(row: FixtureRow) {
  return {
    id: AUDIENCE_ID,
    key: AUDIENCE_ID,
    type: ConfigAudienceTypes.TRANSIENT,
    status: 'active',
    rules: {
      OR: [
        {
          AND: [
            {
              OR_WHEN: [
                {
                  ...fixture.ruleDefaults,
                  matching: {
                    ...fixture.ruleDefaults.matching,
                    negated: row.negated
                  },
                  value: row.ruleValue
                }
              ]
            }
          ]
        }
      ]
    }
  };
}

/**
 * Builds a fresh DataManager for one fixture row: clones the fixture's exp-a/exp-b
 * config, attaches the row's exclusion audience to `exp-b` under `matching_options:
 * ALL` (single audience, so ALL/ANY are equivalent here -- AC6's combination semantics
 * are covered at the js-sdk integration layer instead), and seeds the row's stored
 * bucketing either in-memory (`putData`) or, for row 8, ONLY into a DataStore double
 * (bypassing in-memory entirely, per the fixture's `dataStoreOnly` flag).
 */
function buildRowDataManager(row: FixtureRow, visitorId: string) {
  const logger = new SpyLogger();
  const audience = buildExclusionAudience(row);
  const experiences = (
    fixture.config.experiences as unknown as ConfigExperience[]
  ).map((experience) =>
    experience.key === EXPERIENCE_UNDER_TEST_KEY
      ? {
          ...experience,
          audiences: [AUDIENCE_ID],
          settings: {
            matching_options: {audiences: GenericListMatchingOptions.ALL}
          }
        }
      : experience
  );
  const config = {
    data: {
      account_id: ACCOUNT_ID,
      project: {id: PROJECT_ID},
      experiences,
      audiences: [audience]
    }
  } as unknown as ConfigType;

  const dataManager = new dm(config, {
    bucketingManager,
    ruleManager,
    eventManager,
    apiManager,
    loggerManager: logger as any
  });

  let dataStore: FixtureDataStore | null = null;
  if (Object.keys(row.storedBucketing).length) {
    if (row.dataStoreOnly) {
      dataStore = new FixtureDataStore();
      dataManager.setDataStore(dataStore);
      const storeKey = dataManager.getStoreKey(visitorId);
      // Write directly through the DataStoreManager, bypassing DataManager.putData
      // entirely -- proves the bucketing state is visible ONLY via the DataStore,
      // never via the in-memory `_bucketedVisitors` map (row 8's contract).
      dataManager.dataStoreManager.set(storeKey, {bucketing: row.storedBucketing});
    } else {
      dataManager.putData(visitorId, {bucketing: row.storedBucketing});
    }
  }

  return {dataManager, logger};
}

describe('Mutual-exclusion audience rule (bucketed_into_experience_key) -- qs-03 fixture (AC1, AC4, AC5, AC8)', function () {
  // eslint-disable-next-line mocha/no-setup-in-describe
  ROWS.forEach((row) => {
    it(`row ${row.row}: ${row.description}`, function () {
      const visitorId = `visitor-row-${row.row}`;
      const {dataManager, logger} = buildRowDataManager(row, visitorId);

      // AC5 read-only spies -- armed AFTER seeding, so this row's setup writes/reads
      // are excluded and only the evaluation itself is measured.
      let putDataCallCount = 0;
      const originalPutData = dataManager.putData.bind(dataManager);
      dataManager.putData = ((...args: Parameters<typeof dataManager.putData>) => {
        putDataCallCount++;
        return originalPutData(...args);
      }) as typeof dataManager.putData;

      const targetExperience = (
        fixture.config.experiences as Array<{key: string; id: string}>
      ).find((experience) => experience.key === row.ruleValue);
      let targetRetrieveVariationCallCount = 0;
      if (targetExperience) {
        // `retrieveVariation` is a private method -- accessed via `any` the same way
        // data-manager-preview-decision.tests.ts reaches into `_bucketedVisitors`
        // directly; TS `private` is compile-time only.
        const originalRetrieveVariation = (
          dataManager as any
        ).retrieveVariation.bind(dataManager);
        (dataManager as any).retrieveVariation = (
          experienceId: string,
          variationId: string
        ) => {
          if (String(experienceId) === String(targetExperience.id)) {
            targetRetrieveVariationCallCount++;
          }
          return originalRetrieveVariation(experienceId, variationId);
        };
      }

      // AC4: driven with an EMPTY visitorProperties object throughout.
      const result = dataManager.matchRulesByField(
        visitorId,
        EXPERIENCE_UNDER_TEST_KEY,
        'key',
        {visitorProperties: {}, ignoreLocationProperties: true}
      );

      expect(Boolean(result), `row ${row.row} matched outcome`).to.equal(
        row.expectedMatched
      );

      // AC5 -- read-only: no bucketing of the target, no store write triggered by the
      // exclusion evaluation itself.
      expect(
        putDataCallCount,
        `row ${row.row} putData call count (AC5 read-only)`
      ).to.equal(0);
      if (targetExperience) {
        expect(
          targetRetrieveVariationCallCount,
          `row ${row.row} retrieveVariation(target) call count (AC5 read-only, no bucketing of target)`
        ).to.equal(0);
      }

      // AC8 -- rows 6/7 (unknown target "exp-zz") must warn, naming the unresolved key.
      if (row.expectWarn) {
        const warnedForTarget = logger.warnCalls.some((args) =>
          args.some(
            (arg) => typeof arg === 'string' && arg.includes(row.ruleValue)
          )
        );
        expect(
          warnedForTarget,
          `row ${row.row} expected a warning naming "${row.ruleValue}"`
        ).to.equal(true);
      } else {
        expect(
          logger.warnCalls.length,
          `row ${row.row} expected no warning`
        ).to.equal(0);
      }
    });
  });

  it('loaded the full 8-row cross-SDK fixture (AC1 completeness guard)', function () {
    expect(ROWS).to.have.lengthOf(8);
    expect(ROWS.filter((row) => row.expectedMatched)).to.have.lengthOf(4);
    expect(ROWS.filter((row) => row.expectWarn)).to.have.lengthOf(2);
  });
});
