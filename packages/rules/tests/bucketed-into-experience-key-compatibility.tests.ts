/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * qs-03 (SDK-4) — regression lock for the RuleManager compatibility contract when it receives
 * the new `bucketed_into_experience_key` audience rule type.
 *
 * Spec of record: _bmad-output/planning-artifacts/2026-07-02-convert-js-sdk/qs-03-mutual-exclusion-rule.md
 * Compatibility section (lines 96-98) + AC7 (line 89).
 *
 * `packages/rules/src/rule-manager.ts` is intentionally left BYTE-FOR-BYTE UNCHANGED by qs-03 —
 * the new rule type is resolved entirely in `packages/data`'s DataManager, which reads stored
 * bucketing state before the tree ever reaches RuleManager. This spec locks the documented
 * fall-through contract an old SDK version / a standalone RuleManager exhibits when the rule
 * still reaches it: with `visitorProperties = {}`, `_processRuleItem` never recognizes
 * `rule_type: 'bucketed_into_experience_key'` in literal-object mode, so every row falls through
 * to the final `return false` with `matching.negated` UNAPPLIED (fail-closed, no negation flip) —
 * regardless of the row's `negated` flag or its DataManager-resolved `expectedMatched` value.
 * The DataManager-resolved contract (where this rule actually works) is asserted separately in
 * packages/data/tests.
 *
 * Fixture home: packages/data/tests/mutual-exclusion-rule-fixture.json (owned there — its
 * `config.experiences` blocks and DataManager-facing shape are data-manager concerns). Read here
 * via a relative filesystem path, mirroring packages/data/tests/cross-sdk-vectors.tests.ts's read
 * of the bucketing package's fixture — no new build-graph edge between packages/rules and
 * packages/data is introduced.
 */
import 'mocha';
import {expect} from 'chai';
import * as fs from 'fs';
import * as path from 'path';
import {RuleManager as rm} from '../src/rule-manager';

interface MutualExclusionFixtureRow {
  row: number;
  description: string;
  negated: boolean;
  ruleValue: string;
}

interface MutualExclusionFixture {
  rows: MutualExclusionFixtureRow[];
}

// Hoisted once: shared RuleManager instance + fixture read, per this repo's SonarCloud
// new_duplicated_lines_density <= 3% rule (no per-case setup/teardown, single parameterized loop).
const FIXTURE_PATH = path.resolve(
  __dirname,
  '../../data/tests/mutual-exclusion-rule-fixture.json'
);
const FIXTURE: MutualExclusionFixture = JSON.parse(
  fs.readFileSync(FIXTURE_PATH, 'utf8')
);

// Untyped (implicit `any`), matching rule-manager.tests.ts's own `let ruleManager;` -- the
// fullstack literal-mode rule trees this suite builds (both the `key`-addressed generic vector
// and the `rule_type`-addressed mutual-exclusion rule below) are a fullstack-SDK-only shape and
// are not, and were never meant to be, members of the auto-generated `RuleElement` union in
// packages/types/src/config/ (generated from the OpenAPI spec, never hand-edited). Declaring
// `ruleManager` with its strict `RuleManager` type would force every literal `isRuleMatched`
// call site to fight that generated union instead of exercising the literal-object mode itself.
const ruleManager: any = new rm();

/**
 * Builds the same OR -> AND -> OR_WHEN audience tree shape existing rules tests use (see
 * rule-manager.tests.ts's testRuleSet1/2/3), with a single `bucketed_into_experience_key`
 * rule item as the leaf, exactly as it would be served in a config audience.
 */
function buildMutualExclusionAudienceTree(row: MutualExclusionFixtureRow) {
  return {
    OR: [
      {
        AND: [
          {
            OR_WHEN: [
              {
                rule_type: 'bucketed_into_experience_key',
                matching: {
                  match_type: 'equals',
                  negated: row.negated
                },
                value: row.ruleValue
              }
            ]
          }
        ]
      }
    ]
  };
}

describe('RuleManager compatibility contract for bucketed_into_experience_key (qs-03 SDK-4)', function () {
  // eslint-disable-next-line mocha/no-setup-in-describe
  FIXTURE.rows.forEach((row) => {
    it(`row ${row.row} (negated=${row.negated}, value=${row.ruleValue}): fail-closed fall-through, negation unapplied -- ${row.description}`, function () {
      const tree = buildMutualExclusionAudienceTree(row);
      expect(ruleManager.isRuleMatched({}, tree)).to.equal(false);
    });
  });

  it('AC7 -- generic key/value rule matching is unchanged by the unmodified RuleManager (representative vector)', function () {
    // The 3 generic key/value rule types are exhaustively covered by rule-manager.tests.ts
    // already (equals/less/isIn/isTypeOf/exists/doesNotExist, AND/OR/OR_WHEN combination,
    // negation). This is a targeted, non-duplicating statement that the qs-03 resolution seam
    // -- which lives entirely in DataManager -- left this generic literal-object code path
    // bit-identical: a plain `key`-addressed rule still matches/negates exactly as before.
    const genericRuleSet = {
      OR: [
        {
          AND: [
            {
              OR_WHEN: [
                {
                  key: 'device',
                  matching: {
                    match_type: 'equals',
                    negated: false
                  },
                  value: 'pc'
                }
              ]
            }
          ]
        }
      ]
    };
    expect(ruleManager.isRuleMatched({device: 'pc'}, genericRuleSet)).to.equal(
      true
    );
    expect(
      ruleManager.isRuleMatched({device: 'phone'}, genericRuleSet)
    ).to.equal(false);
  });
});
