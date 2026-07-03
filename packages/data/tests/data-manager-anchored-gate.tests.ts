/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * qs-01 — anchored-vs-packed GATE tests in DataManager._retrieveBucketing.
 *
 * Spec of record: _bmad-output/planning-artifacts/2026-07-02-convert-js-sdk/qs-01-anchored-bucketing-layout.md
 * "The contract (normative)" section (the Gate paragraph + the allocation-build mapping),
 * AC1, AC4, AC8, AC9.
 *
 * These tests lock the shipped gate in DataManager._retrieveBucketing's fresh-bucketing
 * branch (packages/data/src/data-manager.ts:685): `Number(experience.version) > 8` routes
 * fresh bucketing through the anchored layout (built by `_buildVariationAllocations`,
 * data-manager.ts:586, and resolved via `BucketingManager.getBucketForVisitorAnchored`);
 * version <= 8, missing/undefined, or non-numeric version keeps the existing packed
 * cumulative walk unchanged (built by `_buildPackedBuckets`, data-manager.ts:556, and
 * resolved via `BucketingManager.getBucketForVisitor`). AC1 asserts the gate actually
 * branches by exploiting a real packed-vs-anchored disagreement on the same fixture (see
 * below). AC4 asserts stops only zero their own width under the anchored path and never
 * move neighboring arms' anchors. AC8/AC9 protect existing behavior that runs entirely
 * before/independently of the gate: the stored-decision guard (data-manager.ts:658-665)
 * short-circuits before the version check is ever reached, and the returned
 * BucketedVariation shape is unchanged across versions.
 *
 * Fixture derivation methodology: every raw bucket VALUE used below (e.g. 4957, 9807, 102)
 * is a real MurmurHash3 output from BucketingManager.getValueVisitorBased -- the same,
 * already-implemented, already-unit-tested hash oracle the packed and anchored paths both
 * call unmodified (packages/bucketing/src/bucketing-manager.ts:91-112; see
 * packages/bucketing/tests/bucketing-manager.tests.ts for its own tests). Values were
 * derived once (for the fixed (visitorId, experienceId) string pairs used below, with the
 * default hash seed) via that exact method, then frozen as literal numbers so every
 * packed/anchored expectation in this file is independently re-derivable by hand from the
 * spec's normative formulas (anchor = (cumWeight / totalWeight) * 10000; width = active ?
 * allocation * 100 : 0) against the fixed traffic_allocation values declared alongside them,
 * without needing to re-run any tool. This is the same class of technique used in
 * bucketing-manager-anchored.tests.ts to freeze its THIRDS_15/THIRDS_25 expected ranges
 * from the spec's own pseudocode, applied one level up (the raw hash value is looked up
 * from the hash oracle instead of hand-computed, since MurmurHash3 output cannot reasonably
 * be hand-derived; everything downstream of that single looked-up number -- which bucket it
 * falls into under each layout -- is plain arithmetic against the fixed weights,
 * independently checkable inline in each fixture's comment).
 */
import 'mocha';
import {expect} from 'chai';
import {BucketingManager as bm} from '@convertcom/js-sdk-bucketing';
import {RuleManager as rm} from '@convertcom/js-sdk-rules';
import {EventManager as em} from '@convertcom/js-sdk-event';
import {ApiManager as am} from '@convertcom/js-sdk-api';
import {DataManager as dm} from '../src/data-manager';
import testConfig from './test-config.json';
import {
  Config as ConfigType,
  ConfigExperience,
  ExperienceVariationConfig,
  BucketingAttributes
} from '@convertcom/js-sdk-types';
import {objectDeepMerge} from '@convertcom/js-sdk-utils';
import {defaultConfig} from '../../js-sdk/src/config/default';
import {BucketingError} from '@convertcom/js-sdk-enums';

// --- Shared dependency managers (mirrors data-manager.tests.ts's construction pattern) ---
const configuration = objectDeepMerge(
  testConfig,
  defaultConfig,
  {}
) as unknown as ConfigType;
const bucketingManager = new bm(configuration);
const ruleManager = new rm(configuration);
const eventManager = new em(configuration);
const apiManager = new am(configuration, {eventManager});

// Every getBucketing() call in this file targets the GATE only -- not audience/location
// matching, not the tracking queue, not visitor-property persistence. `ignoreLocationProperties`
// bypasses site_area/locations entirely; a truthy (empty) `visitorProperties` object plus an
// empty `experience.audiences` list satisfies matchRulesByField's "unrestricted" branches
// (data-manager.ts:291-334, :350-416); `enableTracking: false` avoids depending on a live
// track-endpoint server for tests that only assert the returned bucketing decision.
const ATTRS: BucketingAttributes = {
  visitorProperties: {},
  ignoreLocationProperties: true,
  enableTracking: false,
  updateVisitorProperties: false
};

function makeVariation(
  id: string,
  trafficAllocation: number,
  status?: 'running' | 'stopped'
): ExperienceVariationConfig {
  return {
    id,
    key: `${id}-key`,
    name: id,
    traffic_allocation: trafficAllocation,
    ...(status ? {status} : {})
  } as unknown as ExperienceVariationConfig;
}

function makeExperience(
  id: string,
  version: number | string | undefined,
  variations: ExperienceVariationConfig[]
): ConfigExperience {
  return {
    id,
    name: `exp-${id}`,
    key: `key-${id}`,
    type: 'a/b_fullstack',
    audiences: [],
    goals: [],
    variations,
    version
  } as unknown as ConfigExperience;
}

function makeDataManager(experiences: ConfigExperience[]) {
  const config = {
    data: {
      account_id: 'anchor-gate-account',
      project: {id: 'anchor-gate-project'},
      experiences
    }
  } as unknown as ConfigType;
  return new dm(config, {
    bucketingManager,
    ruleManager,
    eventManager,
    apiManager
  });
}

function bucketFor(
  dataManager: ReturnType<typeof makeDataManager>,
  visitorId: string,
  experienceId: string
) {
  return dataManager.getBucketingById(visitorId, experienceId, ATTRS);
}

describe('DataManager anchored-vs-packed GATE tests (qs-01 / DATA-1)', function () {
  // --- AC1: gate branching on a real packed-vs-anchored disagreement ---
  // Experience: O=2%, V1=47%, V2=1% (totalWeight=50%, sub-100% -- exactly the shape that
  // makes packed and anchored diverge, per the spec's own "Problem" table).
  //   packed  cumulative walk: O[0,200) V1[200,4900) V2[4900,5000)
  //   anchored ranges (T=50): O anchor=0 width=200 -> [0,200)
  //                           V1 anchor=(2/50)*10000=400   width=4700 -> [400,5100)
  //                           V2 anchor=(49/50)*10000=9800 width=100  -> [9800,9900)
  // visitorId 'anchor-gate-visitor-17' against experienceId '900000001' hashes (via the
  // existing, unmodified BucketingManager.getValueVisitorBased) to the raw value 4957.
  //   4957 is in packed's V2 band [4900,5000)   -> packed expects 'V2'
  //   4957 is in anchored's V1 band [400,5100)  -> anchored expects 'V1'
  const GATE_EXPERIENCE_ID = '900000001';
  const GATE_VISITOR_ID = 'anchor-gate-visitor-17';
  // Inlined (rather than built via makeVariation()) so this describe-level fixture doesn't
  // trip mocha/no-setup-in-describe -- matches the plain-object-literal convention already
  // used for fixtures in bucketing-manager-anchored.tests.ts's THIRDS_15/THIRDS_25.
  const GATE_VARIATIONS: ExperienceVariationConfig[] = [
    {
      id: 'O',
      key: 'O-key',
      name: 'O',
      traffic_allocation: 2,
      status: 'running'
    },
    {
      id: 'V1',
      key: 'V1-key',
      name: 'V1',
      traffic_allocation: 47,
      status: 'running'
    },
    {
      id: 'V2',
      key: 'V2-key',
      name: 'V2',
      traffic_allocation: 1,
      status: 'running'
    }
  ] as unknown as ExperienceVariationConfig[];
  const PACKED_EXPECTED_VARIATION_ID = 'V2';
  const ANCHORED_EXPECTED_VARIATION_ID = 'V1';

  const GATE_CASES: Array<{
    label: string;
    version: number | string | undefined;
    expectedVariationId: string;
  }> = [
    {
      label:
        'version 9 (> 8) routes fresh bucketing through the anchored layout',
      version: 9,
      expectedVariationId: ANCHORED_EXPECTED_VARIATION_ID
    },
    {
      label:
        'version 42 (> 8) routes fresh bucketing through the anchored layout',
      version: 42,
      expectedVariationId: ANCHORED_EXPECTED_VARIATION_ID
    },
    {
      label:
        'version 8 (not > 8) routes fresh bucketing through the packed layout',
      version: 8,
      expectedVariationId: PACKED_EXPECTED_VARIATION_ID
    },
    {
      label:
        'missing/undefined version routes fresh bucketing through the packed layout',
      version: undefined,
      expectedVariationId: PACKED_EXPECTED_VARIATION_ID
    },
    {
      label:
        'non-numeric version (Number(v) -> NaN, NaN > 8 === false) routes fresh bucketing through the packed layout',
      version: 'not-a-number',
      expectedVariationId: PACKED_EXPECTED_VARIATION_ID
    }
  ];

  describe('AC1 -- gate branching (Number(experience.version) > 8)', function () {
    // eslint-disable-next-line mocha/no-setup-in-describe
    GATE_CASES.forEach(({label, version, expectedVariationId}) => {
      it(label, function () {
        const dataManager = makeDataManager([
          makeExperience(GATE_EXPERIENCE_ID, version, GATE_VARIATIONS)
        ]);
        const result = bucketFor(
          dataManager,
          GATE_VISITOR_ID,
          GATE_EXPERIENCE_ID
        );
        expect(result)
          .to.be.an('object')
          .that.has.property('id', expectedVariationId);
      });
    });
  });

  // --- AC4: stops don't move anchors ---
  describe('AC4 -- stops zero only their own width; other arms are byte-identical before/after', function () {
    it('status: stopped (traffic_allocation preserved) zeroes only the width of V1; O and V2 are unaffected', function () {
      // O=10%, V1=80%, V2=10% (totalWeight=100% -- at exactly 100%, packed and anchored
      // coincide in the RUNNING state, isolating the effect of stopping V1 from any
      // sub-100%-driven divergence already covered by the AC1 fixture above).
      //   RUNNING packed:   O[0,1000) V1[1000,9000) V2[9000,10000)
      //   RUNNING anchored: O[0,1000) V1[1000,9000) V2[9000,10000)  (same -- T=100%)
      //   STOPPED packed (V1 excluded from the buckets entirely): O[0,1000) V2[1000,2000)
      //   STOPPED anchored (V1 keeps its weight=80 for anchor stability, width=0):
      //     O anchor=0 width=1000 -> [0,1000) (unchanged)
      //     V1 anchor=(10/100)*10000=1000 width=0 (inactive) -> never selected
      //     V2 anchor=(10+80)/100*10000=9000 width=1000 -> [9000,10000) (unchanged)
      // Both DataManager instances below are fully independent (separate in-memory stores),
      // so reusing the SAME experience id across both is required (not just safe) here: the
      // pre-derived hash values below (102, 9807, 4957) were looked up against experienceId
      // '900000001' specifically -- MurmurHash3's input is `experienceId + visitorId`, so a
      // different id string would hash to different, undetermined values.
      const experienceIdRunning = GATE_EXPERIENCE_ID;
      const experienceIdStopped = GATE_EXPERIENCE_ID;
      const runningVariations = [
        makeVariation('O', 10, 'running'),
        makeVariation('V1', 80, 'running'),
        makeVariation('V2', 10, 'running')
      ];
      const stoppedVariations = [
        makeVariation('O', 10, 'running'),
        makeVariation('V1', 80, 'stopped'),
        makeVariation('V2', 10, 'running')
      ];
      const dataManagerRunning = makeDataManager([
        makeExperience(experienceIdRunning, 9, runningVariations)
      ]);
      const dataManagerStopped = makeDataManager([
        makeExperience(experienceIdStopped, 9, stoppedVariations)
      ]);

      // O witness (value 102, O's own band [0,1000) is always first -> always unaffected).
      const oVisitor = 'anchor-gate-visitor-106';
      expect(bucketFor(dataManagerRunning, oVisitor, experienceIdRunning))
        .to.be.an('object')
        .that.has.property('id', 'O');
      expect(bucketFor(dataManagerStopped, oVisitor, experienceIdStopped))
        .to.be.an('object')
        .that.has.property('id', 'O');

      // V2 witness (value 9807, V2's anchored band [9000,10000) must stay identical once V1
      // stops; packed's cumulative walk reshuffles V2 down to [1000,2000) once V1 is excluded,
      // so this is the assertion that fails today (pre-gate, still packed for version 9).
      const v2Visitor = 'anchor-gate-visitor-162';
      expect(bucketFor(dataManagerRunning, v2Visitor, experienceIdRunning))
        .to.be.an('object')
        .that.has.property('id', 'V2');
      expect(bucketFor(dataManagerStopped, v2Visitor, experienceIdStopped))
        .to.be.an('object')
        .that.has.property('id', 'V2');

      // V1 witness (value 4957): bucketed into the arm being stopped while it was running,
      // and correctly unselectable (zero width) once stopped.
      const v1Visitor = GATE_VISITOR_ID;
      expect(bucketFor(dataManagerRunning, v1Visitor, experienceIdRunning))
        .to.be.an('object')
        .that.has.property('id', 'V1');
      expect(
        bucketFor(dataManagerStopped, v1Visitor, experienceIdStopped)
      ).to.equal(BucketingError.VARIAION_NOT_DECIDED);
    });

    it('explicit traffic_allocation: 0 is zero-width (never defaults to 100) and does not perturb neighboring anchors', function () {
      // Same O/V1/V2 weights and experienceId as the AC1 fixture, with an explicit
      // Z (traffic_allocation: 0, status: running) inserted between V1 and V2. A zero-weight
      // entry contributes nothing to cumWeight/totalWeight under the spec's formula, so it
      // must be mathematically inert to every OTHER arm's anchor -- V1 and V2's expectations
      // are identical to the AC1 fixture; Z itself must never be selected.
      //   totalWeight = 2 + 47 + 0 + 1 = 50 (same as AC1)
      //   anchored: O[0,200)  V1 anchor=400 width=4700 -> [400,5100)
      //             Z  anchor=(49/50)*10000=9800 width=0 (ta=0 -> inactive) -> never selected
      //             V2 anchor=9800 width=100 -> [9800,9900) (shares Z's anchor, unaffected)
      const variationsWithZero = [
        makeVariation('O', 2, 'running'),
        makeVariation('V1', 47, 'running'),
        makeVariation('Z', 0, 'running'),
        makeVariation('V2', 1, 'running')
      ];
      const dataManager = makeDataManager([
        makeExperience(GATE_EXPERIENCE_ID, 9, variationsWithZero)
      ]);

      const v1Visitor = GATE_VISITOR_ID; // value 4957 -> anchored V1's band
      const v2Visitor = 'anchor-gate-visitor-162'; // value 9807 -> anchored V2's band
      const oVisitor = 'anchor-gate-visitor-106'; // value 102 -> anchored O's band

      const v1Result = bucketFor(dataManager, v1Visitor, GATE_EXPERIENCE_ID);
      const v2Result = bucketFor(dataManager, v2Visitor, GATE_EXPERIENCE_ID);
      const oResult = bucketFor(dataManager, oVisitor, GATE_EXPERIENCE_ID);

      expect(v1Result).to.be.an('object').that.has.property('id', 'V1');
      expect(v2Result).to.be.an('object').that.has.property('id', 'V2');
      expect(oResult).to.be.an('object').that.has.property('id', 'O');

      // Z (traffic_allocation: 0) must never be the bucketed variation for any of the above.
      [v1Result, v2Result, oResult].forEach((result) => {
        if (result && typeof result === 'object') {
          expect((result as {id?: string}).id).to.not.equal('Z');
        }
      });
    });
  });

  // --- AC8: guard precedence ---
  describe('AC8 -- a stored decision wins over the anchored path', function () {
    it('returns the previously stored variation for a version-9 experience even though the anchored path would pick a different arm', function () {
      const dataManager = makeDataManager([
        makeExperience(GATE_EXPERIENCE_ID, 9, GATE_VARIATIONS)
      ]);
      // Seed a stored decision ('O') that disagrees with what the anchored path would
      // naturally compute for this visitor (ANCHORED_EXPECTED_VARIATION_ID === 'V1').
      dataManager.putData(GATE_VISITOR_ID, {
        bucketing: {[GATE_EXPERIENCE_ID]: 'O'}
      });

      const result = bucketFor(
        dataManager,
        GATE_VISITOR_ID,
        GATE_EXPERIENCE_ID
      );
      expect(result).to.be.an('object').that.has.property('id', 'O');
      expect((result as {id?: string}).id).to.not.equal(
        ANCHORED_EXPECTED_VARIATION_ID
      );
    });
  });

  // --- AC9: no event/schema drift ---
  describe('AC9 -- BucketedVariation shape is unchanged between version 8 and version 9', function () {
    it('returns structurally identical keys for a single-arm, 100%-allocation experience regardless of version', function () {
      const singleArmVariations = [makeVariation('A', 100, 'running')];
      const dataManagerV8 = makeDataManager([
        makeExperience('gate-exp-schema-v8', 8, singleArmVariations)
      ]);
      const dataManagerV9 = makeDataManager([
        makeExperience('gate-exp-schema-v9', 9, singleArmVariations)
      ]);
      const visitorId = 'anchor-gate-visitor-schema';

      const resultV8 = bucketFor(
        dataManagerV8,
        visitorId,
        'gate-exp-schema-v8'
      );
      const resultV9 = bucketFor(
        dataManagerV9,
        visitorId,
        'gate-exp-schema-v9'
      );

      expect(resultV8).to.be.an('object');
      expect(resultV9).to.be.an('object');
      expect(
        Object.keys(resultV9 as object).sort((a, b) => a.localeCompare(b))
      ).to.deep.equal(
        Object.keys(resultV8 as object).sort((a, b) => a.localeCompare(b))
      );
    });
  });
});
