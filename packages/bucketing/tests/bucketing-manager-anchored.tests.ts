/* eslint-disable mocha/consistent-spacing-between-blocks */
/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * qs-01 — anchored bucketing algorithm tests.
 *
 * Spec of record: _bmad-output/planning-artifacts/2026-07-02-convert-js-sdk/qs-01-anchored-bucketing-layout.md
 * "The contract (normative)" section, AC2/AC3/AC4/AC5.
 *
 * These tests lock the shipped behavior of the three anchored bucketing methods on
 * BucketingManager (packages/bucketing/src/bucketing-manager.ts): `getBucketRanges`
 * (builds the anchor/width layout from a set of variation allocations), `selectBucketAnchored`
 * (resolves a raw bucket value against that layout via a half-open [anchor, anchor + width)
 * interval), and `getBucketForVisitorAnchored` (reuses the existing visitor-based hash,
 * unmodified, and routes it through the two methods above). Together the suites below verify:
 *   - the raise-superset property: anchors are computed over the total weight of ALL
 *     entries so growing an experience's total allocation only ever grows arms and never
 *     reshuffles an already-bucketed visitor into a different arm (AC2 thirds/superset
 *     fixtures);
 *   - half-open boundary semantics at the anchor and anchor + width edges (AC5);
 *   - zero/inactive-arm handling: stopped or explicit zero-allocation entries keep their
 *     weight for anchor stability but get a zero-width range so they can never be selected,
 *     and a totalWeight <= 0 layout yields no bucketing (AC4/AC5);
 *   - determinism: getBucketForVisitorAnchored returns the same result for the same
 *     (visitorId, experienceId) and matches selectBucketAnchored(getBucketRanges(...), hash)
 *     composed directly from the existing hash oracle.
 *
 * Expected numbers below are derived BY HAND directly from the spec's normative
 * pseudocode (anchor = (cumWeight / totalWeight) * 10000; width = active ? allocation * 100 : 0),
 * written as literal arithmetic expressions so the doubles are IEEE754-exact and
 * independently verifiable against the spec's layout table — never computed by calling
 * the SUT.
 */
import 'mocha';
import {expect} from 'chai';
import {BucketingManager as bm} from '../src/bucketing-manager';
import {BucketAnchoredRange} from '../src/interfaces/bucketing-manager';
import {VariationAllocation} from '@convertcom/js-sdk-types';

// --- AC2: anchored thirds fixtures (Distilled.ie incident: 3 equal arms O/V1/V2) ---
// Spec table (qs-01 "Problem"):
//   anchored 15%: O [0,500)     V1 [3333,3833)  V2 [6667,7167)
//   anchored 25%: O [0,833)     V1 [3333,4166)  V2 [6667,7500)
// "15%"/"25%" is the experience's TOTAL coverage split evenly across 3 arms; the table's
// integers are display-rounded, the expected values here are the exact doubles the
// formula produces.
const THIRDS_15: {
  label: string;
  allocations: VariationAllocation[];
  expectedRanges: BucketAnchoredRange[];
} = {
  label: '15% total coverage (thirds: 5% per arm)',
  allocations: [
    {id: 'O', allocation: 5, active: true},
    {id: 'V1', allocation: 5, active: true},
    {id: 'V2', allocation: 5, active: true}
  ],
  expectedRanges: [
    {id: 'O', anchor: (0 / 15) * 10000, width: 5 * 100},
    {id: 'V1', anchor: (5 / 15) * 10000, width: 5 * 100},
    {id: 'V2', anchor: (10 / 15) * 10000, width: 5 * 100}
  ]
};

const THIRDS_25: {
  label: string;
  allocations: VariationAllocation[];
  expectedRanges: BucketAnchoredRange[];
} = {
  label: '25% total coverage (thirds: 25/3% per arm)',
  allocations: [
    {id: 'O', allocation: 25 / 3, active: true},
    {id: 'V1', allocation: 25 / 3, active: true},
    {id: 'V2', allocation: 25 / 3, active: true}
  ],
  expectedRanges: [
    {id: 'O', anchor: (0 / 25) * 10000, width: (25 / 3) * 100},
    {id: 'V1', anchor: (25 / 3 / 25) * 10000, width: (25 / 3) * 100},
    {
      id: 'V2',
      anchor: ((2 * (25 / 3)) / 25) * 10000,
      width: (25 / 3) * 100
    }
  ]
};

const THIRDS_SCENARIOS = [THIRDS_15, THIRDS_25];

// Per-sliver / raise-superset admission table (AC2): one value per growth sliver, plus
// one value already inside the 15% layout to prove it never flips arm at 25%.
const SUPERSET_CASES: Array<{
  value: number;
  at15: string | null;
  at25: string;
  description: string;
}> = [
  {
    value: 100,
    at15: 'O',
    at25: 'O',
    description:
      'a visitor already bucketed into O at 15% keeps O at 25% (superset, never flips)'
  },
  {
    value: 600,
    at15: null,
    at25: 'O',
    description:
      'the O growth sliver [500,833.33) is unbucketed at 15% and newly admitted into O at 25%'
  },
  {
    value: 4000,
    at15: null,
    at25: 'V1',
    description:
      'the V1 growth sliver (3833.33,4166.67) is unbucketed at 15% and newly admitted into V1 at 25%'
  },
  {
    value: 7300,
    at15: null,
    at25: 'V2',
    description:
      'the V2 growth sliver (7166.67,7500) is unbucketed at 15% and newly admitted into V2 at 25%'
  }
];

// AC5: anchor/width boundary semantics on a single-arm range, independent of getBucketRanges.
const BOUNDARY_RANGES: BucketAnchoredRange[] = [
  {id: 'A', anchor: 1000, width: 500}
];
const BOUNDARY_CASES: Array<{
  value: number;
  expected: string | null;
  description: string;
}> = [
  {value: 999, expected: null, description: 'value just below anchor is OUT'},
  {value: 1000, expected: 'A', description: 'value === anchor is IN'},
  {
    value: 1499,
    expected: 'A',
    description: 'value just below anchor + width is IN'
  },
  {
    value: 1500,
    expected: null,
    description: 'value === anchor + width is OUT (falls through, no arm)'
  }
];

// AC5: totalWeight <= 0 -> not bucketed (asserted at the top-level getBucketForVisitorAnchored
// seam per the spec's own phrasing: "getBucketForVisitorAnchored (or the range/select path)").
const ZERO_TOTAL_WEIGHT_CASES: Array<{
  label: string;
  allocations: VariationAllocation[];
}> = [
  {label: 'empty allocations array', allocations: []},
  {
    label: 'every entry has allocation 0 (active and inactive)',
    allocations: [
      {id: 'A', allocation: 0, active: true},
      {id: 'B', allocation: 0, active: false}
    ]
  }
];

describe('BucketingManager anchored tests (qs-01 / BUCK-2 — contract v9 anchored algorithm)', function () {
  // Matches the existing packed-path suite's convention: no explicit type annotation
  // is needed since the anchored methods are implemented directly on the
  // BucketingManager class.
  let bucketingManager;

  beforeEach(function () {
    bucketingManager = new bm();
  });

  describe('getBucketRanges() — AC2 anchored thirds layout (superset property)', function () {
    // eslint-disable-next-line mocha/no-setup-in-describe
    THIRDS_SCENARIOS.forEach(({label, allocations, expectedRanges}) => {
      it(`computes the exact anchors/widths at ${label}`, function () {
        expect(bucketingManager.getBucketRanges(allocations)).to.deep.equal(
          expectedRanges
        );
      });
    });
  });

  describe('selectBucketAnchored() — AC2 raise-superset and per-sliver admission', function () {
    // eslint-disable-next-line mocha/no-setup-in-describe
    SUPERSET_CASES.forEach(({value, at15, at25, description}) => {
      it(description, function () {
        expect(
          bucketingManager.selectBucketAnchored(THIRDS_15.expectedRanges, value)
        ).to.equal(at15);
        expect(
          bucketingManager.selectBucketAnchored(THIRDS_25.expectedRanges, value)
        ).to.equal(at25);
      });
    });
  });

  describe('selectBucketAnchored() — AC5 anchor/width boundary semantics (half-open interval)', function () {
    // eslint-disable-next-line mocha/no-setup-in-describe
    BOUNDARY_CASES.forEach(({value, expected, description}) => {
      it(description, function () {
        expect(
          bucketingManager.selectBucketAnchored(BOUNDARY_RANGES, value)
        ).to.equal(expected);
      });
    });

    it('selectBucketAnchored on an empty ranges array always yields null', function () {
      expect(bucketingManager.selectBucketAnchored([], 4242)).to.equal(null);
    });
  });

  describe('AC5 — defaults', function () {
    it('an allocation of 100 (the isNaN(ta) -> 100.0 default, already applied upstream per DataManager convention) is a normal full-width weight', function () {
      const allocations: VariationAllocation[] = [
        {id: 'A', allocation: 100, active: true}
      ];
      const expectedRanges: BucketAnchoredRange[] = [
        {id: 'A', anchor: 0, width: 10000}
      ];
      expect(bucketingManager.getBucketRanges(allocations)).to.deep.equal(
        expectedRanges
      );
      expect(
        bucketingManager.selectBucketAnchored(expectedRanges, 5000)
      ).to.equal('A');
    });

    // eslint-disable-next-line mocha/no-setup-in-describe
    ZERO_TOTAL_WEIGHT_CASES.forEach(({label, allocations}) => {
      it(`totalWeight <= 0 (${label}) yields null via getBucketForVisitorAnchored`, function () {
        expect(
          bucketingManager.getBucketForVisitorAnchored(allocations, '01ABCD')
        ).to.equal(null);
      });
    });
  });

  describe('AC4 — stops and zero-allocation arms are zero-width but keep their weight', function () {
    it('an inactive arm keeps its weight (later arms have byte-identical anchors) but gets zero width and is never selected', function () {
      const activeAllocations: VariationAllocation[] = [
        {id: 'O', allocation: 10, active: true},
        {id: 'V1', allocation: 10, active: true},
        {id: 'V2', allocation: 10, active: true}
      ];
      const stoppedAllocations: VariationAllocation[] = [
        {id: 'O', allocation: 10, active: true},
        {id: 'V1', allocation: 10, active: false},
        {id: 'V2', allocation: 10, active: true}
      ];

      const activeRanges = bucketingManager.getBucketRanges(activeAllocations);
      const stoppedRanges =
        bucketingManager.getBucketRanges(stoppedAllocations);

      // Anchor stability under status-based stops: stopping V1 changes only V1's width.
      expect(stoppedRanges.map((range) => range.anchor)).to.deep.equal(
        activeRanges.map((range) => range.anchor)
      );
      expect(stoppedRanges[2]).to.deep.equal(activeRanges[2]); // V2 is byte-identical

      expect(stoppedRanges[1].width).to.equal(0);
      expect(
        bucketingManager.selectBucketAnchored(
          stoppedRanges,
          stoppedRanges[1].anchor
        )
      ).to.not.equal('V1');
    });

    it('an explicit allocation: 0 arm is zero-width (never defaults to 100) and is skipped for the next arm sharing its anchor', function () {
      const allocations: VariationAllocation[] = [
        {id: 'O', allocation: 10, active: true},
        {id: 'Z', allocation: 0, active: true},
        {id: 'V2', allocation: 10, active: true}
      ];
      const ranges: BucketAnchoredRange[] =
        bucketingManager.getBucketRanges(allocations);
      const zRange = ranges.find((range) => range.id === 'Z');
      const v2Range = ranges.find((range) => range.id === 'V2');

      expect(zRange.width).to.equal(0);
      expect(zRange.anchor).to.equal(v2Range.anchor);
      expect(
        bucketingManager.selectBucketAnchored(ranges, zRange.anchor)
      ).to.equal('V2');
    });
  });

  describe('getBucketForVisitorAnchored() — reuses the existing visitor hash, routes through getBucketRanges + selectBucketAnchored', function () {
    const visitorId = '01ABCD';
    const options = {experienceId: 'exp-anchored-1'};

    it('is deterministic for the same visitorId/experienceId', function () {
      const first = bucketingManager.getBucketForVisitorAnchored(
        THIRDS_15.allocations,
        visitorId,
        options
      );
      const second = bucketingManager.getBucketForVisitorAnchored(
        THIRDS_15.allocations,
        visitorId,
        options
      );
      expect(second).to.deep.equal(first);
    });

    it('matches selectBucketAnchored(getBucketRanges(allocations), getValueVisitorBased(visitorId, options)) — the existing hash, unmodified', function () {
      const value = bucketingManager.getValueVisitorBased(visitorId, options);
      const expectedVariationId = bucketingManager.selectBucketAnchored(
        bucketingManager.getBucketRanges(THIRDS_15.allocations),
        value
      );
      const result = bucketingManager.getBucketForVisitorAnchored(
        THIRDS_15.allocations,
        visitorId,
        options
      );
      if (expectedVariationId === null) {
        expect(result).to.equal(null);
      } else {
        expect(result).to.deep.equal({
          variationId: expectedVariationId,
          bucketingAllocation: value
        });
      }
    });
  });
});
