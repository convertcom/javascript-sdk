/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * SDK-3 (RED) -- qs-03 mutual-exclusion audience rule (`bucketed_into_experience_key`),
 * end-to-end through the public Context API against the real built CJS bundle.
 *
 * Spec of record: _bmad-output/planning-artifacts/2026-07-02-convert-js-sdk/qs-03-mutual-exclusion-rule.md
 * AC2 (end-to-end exclusion), AC3 (DataStore persistence), AC4 (no new inputs), AC5
 * (read-only), AC6 (ALL/ANY combination semantics).
 *
 * Mirrors full-chain.spec.ts's structure (imports the built `lib/index` the same way a
 * real consumer would, plain-object config, `sdk.onReady()` + `sdk.createContext()`).
 * Unlike full-chain.spec.ts this suite does not need the staging "static"/"live" dual
 * mode -- every case constructs its own minimal two-experience `data` config in-process
 * (`buildConfig()` below), since the scenarios under test (exclusion audiences, ALL/ANY
 * combination) don't exist in the shared staging project.
 *
 * RED-phase note: `data-manager.ts` has no resolution for `bucketed_into_experience_key`
 * yet, so every audience carrying this rule fails closed (see the unit-level fixture
 * suite's docstring in packages/data/tests/mutual-exclusion-rule.tests.ts for the exact
 * mechanism). Concretely, today: AC2/AC3's "excluded" assertions fail because exp-b
 * already returns `null` for EVERY visitor (the audience never matches, regardless of
 * negation), including the fresh visitor who should bucket normally; AC6's ALL/ANY
 * assertions fail for the same reason. This file is authored in the RED phase only --
 * no production code changed.
 */
import {test, expect} from '@playwright/test';

// Import from the built CJS bundle -- same as consumers would use.
// eslint-disable-next-line @typescript-eslint/no-var-requires
const SDK = require('../../lib/index');
const ConvertSDK = SDK.default;
const {SystemEvents} = SDK;

const EXP_A_KEY = 'exp-a';
const EXP_A_ID = '100111';
const EXP_A_VARIATION_ID = '100901';
const EXP_B_KEY = 'exp-b';
const EXP_B_ID = '100222';
const EXP_B_VARIATION_ID = '100902';
const EXCLUSION_AUDIENCE_ID = 'qs-03-exclusion-audience';
const GENERIC_AUDIENCE_ID = 'qs-03-generic-audience';
const GENERIC_KEY = 'plan';
const GENERIC_MATCH_VALUE = 'pro';

// AC4: driven with an empty visitorProperties object throughout (plus
// ignoreLocationProperties, since these experiences carry no location rules).
const RUN_ATTRS = {visitorProperties: {}, ignoreLocationProperties: true};

function makeExclusionAudience(negated: boolean) {
  return {
    id: EXCLUSION_AUDIENCE_ID,
    key: EXCLUSION_AUDIENCE_ID,
    type: 'transient',
    status: 'active',
    rules: {
      OR: [
        {
          AND: [
            {
              OR_WHEN: [
                {
                  rule_type: 'bucketed_into_experience_key',
                  matching: {match_type: 'equals', negated},
                  value: EXP_A_KEY
                }
              ]
            }
          ]
        }
      ]
    }
  };
}

function makeGenericAudience() {
  return {
    id: GENERIC_AUDIENCE_ID,
    key: GENERIC_AUDIENCE_ID,
    type: 'transient',
    status: 'active',
    rules: {
      OR: [
        {
          AND: [
            {
              OR_WHEN: [
                {
                  rule_type: 'generic_key_value',
                  matching: {match_type: 'matches', negated: false},
                  key: GENERIC_KEY,
                  value: GENERIC_MATCH_VALUE
                }
              ]
            }
          ]
        }
      ]
    }
  };
}

// Builds a minimal two-experience config: exp-a carries no audience restriction; exp-b
// carries whichever audiences/matching_options the case under test needs. Hoisted as a
// single builder (per this repo's SonarCloud `new_duplicated_lines_density <= 3%` rule)
// so no case hand-assembles the experience/variation shape.
function buildConfig({
  expBAudienceIds = [],
  matchingOptions = 'all',
  audiences = []
}: {
  expBAudienceIds?: string[];
  matchingOptions?: 'all' | 'any';
  audiences?: Array<Record<string, any>>;
} = {}) {
  return {
    account_id: 'qs-03-account',
    project: {id: 'qs-03-project'},
    experiences: [
      {
        id: EXP_A_ID,
        name: 'exp-a',
        key: EXP_A_KEY,
        type: 'a/b_fullstack',
        status: 'active',
        audiences: [],
        goals: [],
        variations: [
          {
            id: EXP_A_VARIATION_ID,
            name: 'exp-a variation',
            status: 'running',
            is_baseline: true
          }
        ]
      },
      {
        id: EXP_B_ID,
        name: 'exp-b',
        key: EXP_B_KEY,
        type: 'a/b_fullstack',
        status: 'active',
        audiences: expBAudienceIds,
        goals: [],
        settings: {matching_options: {audiences: matchingOptions}},
        variations: [
          {
            id: EXP_B_VARIATION_ID,
            name: 'exp-b variation',
            status: 'running',
            is_baseline: true
          }
        ]
      }
    ],
    audiences
  };
}

// Minimal in-memory DataStore double -- mirrors full-chain.spec.ts's MemoryDataStore,
// with a call counter (mirrors preview-zero-trace.spec.ts's convention) so AC5's
// read-only claim can be measured directly.
class MemoryDataStore {
  data: Record<string, any> = {};
  setCallCount = 0;
  get(key: string): any {
    if (!key) return this.data;
    return this.data[key.toString()];
  }
  set(key: string, value: any): void {
    this.setCallCount++;
    if (!key) throw new Error('Invalid DataStore key!');
    this.data[key.toString()] = value;
  }
}

function createSdk(
  config: Record<string, any>,
  overrides: Record<string, any> = {}
) {
  return new ConvertSDK({
    data: config,
    network: {tracking: false},
    ...overrides
  });
}

async function createReadyContext(
  config: Record<string, any>,
  visitorId: string,
  overrides: Record<string, any> = {}
) {
  const sdk = createSdk(config, overrides);
  await sdk.onReady();
  const context = sdk.createContext(visitorId, {}); // AC4: no visitorAttributes
  return {sdk, context};
}

// Records every SystemEvents.BUCKETING fire (by experienceKey) for the lifetime of an
// sdk instance -- used by AC5 to prove the exclusion evaluation never buckets its
// target as a side effect. MUST be registered before the FIRST bucketing event of the
// test fires: EventManager replays the earliest `deferred: true` firing of an event to
// any listener registered afterwards (packages/event/src/event-manager.ts `on()`), so a
// listener attached mid-test would spuriously "see" an earlier, unrelated bucketing.
function recordBucketingEvents(sdk: any): Array<{experienceKey: string}> {
  const events: Array<{experienceKey: string}> = [];
  sdk.on(SystemEvents.BUCKETING, (args: {experienceKey: string}) =>
    events.push(args)
  );
  return events;
}

test.describe('Mutual-exclusion audience rule end-to-end (qs-03 / SDK-3)', () => {
  test('AC2: fresh visitor bucketed into A is excluded from B; a different visitor who never ran A buckets into B normally', async () => {
    const config = buildConfig({
      expBAudienceIds: [EXCLUSION_AUDIENCE_ID],
      matchingOptions: 'all',
      audiences: [makeExclusionAudience(true)]
    });

    const {context: excludedContext} = await createReadyContext(
      config,
      'visitor-ac2-excluded'
    );
    const decisionA = excludedContext.runExperience(EXP_A_KEY, RUN_ATTRS);
    expect(decisionA).toBeTruthy();
    expect(decisionA.id).toBe(EXP_A_VARIATION_ID);

    const decisionB = excludedContext.runExperience(EXP_B_KEY, RUN_ATTRS);
    expect(decisionB).toBeNull();

    const {context: freshContext} = await createReadyContext(
      config,
      'visitor-ac2-fresh-never-ran-a'
    );
    const decisionBFresh = freshContext.runExperience(EXP_B_KEY, RUN_ATTRS);
    expect(decisionBFresh).toBeTruthy();
    expect(decisionBFresh.id).toBe(EXP_B_VARIATION_ID);
  });

  test('AC3: a DataStore-backed decision from one SDK instance excludes the visitor from B in a brand-new instance/context (fixture row 8, end-to-end)', async () => {
    const config = buildConfig({
      expBAudienceIds: [EXCLUSION_AUDIENCE_ID],
      matchingOptions: 'all',
      audiences: [makeExclusionAudience(true)]
    });
    const dataStore = new MemoryDataStore();
    const visitorId = 'visitor-ac3-datastore';

    const {context: firstContext} = await createReadyContext(
      config,
      visitorId,
      {dataStore}
    );
    const decisionA = firstContext.runExperience(EXP_A_KEY, RUN_ATTRS);
    expect(decisionA).toBeTruthy();

    // A brand-new SDK instance/context, sharing only the DataStore.
    const {context: secondContext} = await createReadyContext(
      config,
      visitorId,
      {dataStore}
    );
    const decisionB = secondContext.runExperience(EXP_B_KEY, RUN_ATTRS);
    expect(decisionB).toBeNull();

    // Control: a THIRD instance/context, sharing the same DataStore but a visitor who
    // never ran A, must still bucket into B normally -- proves the exclusion is keyed
    // off the visitor's actual stored decision (via the DataStore), not a blanket
    // "exp-b never serves" fallback.
    const {context: thirdContext} = await createReadyContext(
      config,
      'visitor-ac3-datastore-control-never-ran-a',
      {dataStore}
    );
    const decisionBControl = thirdContext.runExperience(EXP_B_KEY, RUN_ATTRS);
    expect(decisionBControl).toBeTruthy();
    expect(decisionBControl.id).toBe(EXP_B_VARIATION_ID);
  });

  test('AC5: evaluating the exclusion audience never buckets, stores, or tracks the target experience', async () => {
    const config = buildConfig({
      expBAudienceIds: [EXCLUSION_AUDIENCE_ID],
      matchingOptions: 'all',
      audiences: [makeExclusionAudience(true)]
    });
    const dataStore = new MemoryDataStore();
    const visitorId = 'visitor-ac5-read-only';

    const {sdk, context} = await createReadyContext(config, visitorId, {
      dataStore
    });
    // Registered before ANY bucketing event fires in this test (see
    // recordBucketingEvents()'s docstring on why ordering matters here).
    const bucketingEvents = recordBucketingEvents(sdk);

    const decisionA = context.runExperience(EXP_A_KEY, RUN_ATTRS);
    expect(decisionA).toBeTruthy();
    expect(
      bucketingEvents.filter((event) => event.experienceKey === EXP_A_KEY)
    ).toHaveLength(1);

    const setCallCountBeforeExclusionCheck = dataStore.setCallCount;
    const decisionB = context.runExperience(EXP_B_KEY, RUN_ATTRS);

    expect(decisionB).toBeNull();
    // AC5 -- read-only: evaluating exp-b's exclusion audience must not trigger a
    // SECOND bucketing of the target (exp-a) as a side effect, nor bucket exp-b
    // itself (it's excluded), nor write to the DataStore.
    expect(
      bucketingEvents.filter((event) => event.experienceKey === EXP_A_KEY)
    ).toHaveLength(1);
    expect(
      bucketingEvents.filter((event) => event.experienceKey === EXP_B_KEY)
    ).toHaveLength(0);
    expect(dataStore.setCallCount).toBe(setCallCountBeforeExclusionCheck);
  });

  // AC6 -- combination semantics. Each case is deliberately built so its expectation
  // can ONLY be produced by a correctly-resolved exclusion rule (never by the pre-seam
  // fail-closed-always-false fallback): cases 1 and 3 require the exclusion rule to
  // resolve to `true` (visitor never ran A) for the overall experience to serve, which
  // the fail-closed fallback cannot produce. Case 2 is the negative control (exclusion
  // genuinely violated) to guard against a false-positive "always true" regression.
  interface Ac6Case {
    name: string;
    matchingOptions: 'all' | 'any';
    visitorId: string;
    runAFirst: boolean;
    visitorProperties: Record<string, any>;
    expectServed: boolean;
  }

  const AC6_CASES: Ac6Case[] = [
    {
      name: 'ALL requires BOTH to pass: generic matches AND visitor genuinely excluded (never ran A) -> served',
      matchingOptions: 'all',
      visitorId: 'visitor-ac6-all-served',
      runAFirst: false,
      visitorProperties: {[GENERIC_KEY]: GENERIC_MATCH_VALUE},
      expectServed: true
    },
    {
      name: 'ALL fails when the exclusion half fails: generic matches BUT visitor IS bucketed into A -> not served',
      matchingOptions: 'all',
      visitorId: 'visitor-ac6-all-not-served',
      runAFirst: true,
      visitorProperties: {[GENERIC_KEY]: GENERIC_MATCH_VALUE},
      expectServed: false
    },
    {
      name: 'ANY is satisfied by the exclusion audience alone: generic does not match, visitor never ran A -> served',
      matchingOptions: 'any',
      visitorId: 'visitor-ac6-any-served-by-exclusion-alone',
      runAFirst: false,
      visitorProperties: {},
      expectServed: true
    }
  ];

  // eslint-disable-next-line mocha/no-setup-in-describe
  AC6_CASES.forEach((ac6Case) => {
    test(`AC6: ${ac6Case.name}`, async () => {
      const config = buildConfig({
        expBAudienceIds: [GENERIC_AUDIENCE_ID, EXCLUSION_AUDIENCE_ID],
        matchingOptions: ac6Case.matchingOptions,
        audiences: [makeGenericAudience(), makeExclusionAudience(true)]
      });
      const {context} = await createReadyContext(config, ac6Case.visitorId);

      if (ac6Case.runAFirst) {
        const decisionA = context.runExperience(EXP_A_KEY, RUN_ATTRS);
        expect(decisionA).toBeTruthy();
      }

      const result = context.runExperience(EXP_B_KEY, {
        visitorProperties: ac6Case.visitorProperties,
        ignoreLocationProperties: true
      });

      if (ac6Case.expectServed) {
        expect(result).toBeTruthy();
        expect(result.id).toBe(EXP_B_VARIATION_ID);
      } else {
        expect(result).toBeNull();
      }
    });
  });
});
