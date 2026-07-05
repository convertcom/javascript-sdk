/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * SDK-4 -- DataManager.getPreviewDecision() full-bypass tests (AC4, AC7 variation-half).
 *
 * getPreviewDecision(experience, variationId) is a PURE method: it resolves the requested
 * variation directly from the `experience.variations` array passed in (never through
 * `retrieveVariation`/`getSubItem`, which only read the shared config -- a preview
 * experience may come from a `?exp=` scratch object that is never registered there). It
 * bypasses audiences/segments/locations, the environment check, experience status,
 * variation status/traffic filters, stored decisions, and the bucketing hash entirely, and
 * performs no `putData` and no `apiManager.enqueue`. An unknown variationId is inert (null).
 *
 * RED phase: `getPreviewDecision` does not exist yet on DataManager. Every call below is
 * cast `as any` so the file compiles; the calls themselves fail at runtime (method
 * undefined), which is the expected RED signal.
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
  BucketedVariation,
  ExperienceStatuses,
  VariationStatuses
} from '@convertcom/js-sdk-types';
import {objectDeepMerge} from '@convertcom/js-sdk-utils';
import {defaultConfig} from '../../js-sdk/src/config/default';

// --- Shared dependency managers (mirrors data-manager.tests.ts / anchored-gate's pattern) ---
const configuration = objectDeepMerge(
  testConfig,
  defaultConfig,
  {}
) as unknown as ConfigType;
const bucketingManager = new bm(configuration);
const ruleManager = new rm(configuration);
const eventManager = new em(configuration);
const apiManager = new am(configuration, {eventManager});

// Wrap the shared apiManager's enqueue with a call counter so every test can assert
// getPreviewDecision performs zero tracking side effects, without pulling in sinon (not a
// dependency of this package -- see data-manager.tests.ts / data-manager-anchored-gate.tests.ts,
// neither of which use a mocking library either).
let enqueueCallCount = 0;
const originalEnqueue = apiManager.enqueue.bind(apiManager);
apiManager.enqueue = ((...args: Parameters<typeof apiManager.enqueue>) => {
  enqueueCallCount++;
  return originalEnqueue(...args);
}) as typeof apiManager.enqueue;

const TARGET_VARIATION_ID = 'target-variation';
const OTHER_VARIATION_ID = 'other-variation';
const UNKNOWN_VARIATION_ID = 'does-not-exist-on-experience';
const PREVIEW_VISITOR_ID = 'preview-decision-visitor';

function makeVariation(
  overrides: Partial<ExperienceVariationConfig> = {}
): ExperienceVariationConfig {
  return {
    id: TARGET_VARIATION_ID,
    key: `${TARGET_VARIATION_ID}-key`,
    name: 'Target Variation',
    status: VariationStatuses.RUNNING,
    traffic_allocation: 50,
    ...overrides
  } as unknown as ExperienceVariationConfig;
}

function makeExperience(
  overrides: Partial<ConfigExperience> & {
    variations?: ExperienceVariationConfig[];
  } = {}
): ConfigExperience {
  return {
    id: 'preview-experience',
    key: 'preview-experience-key',
    name: 'Preview Experience',
    type: 'a/b_fullstack',
    status: ExperienceStatuses.ACTIVE,
    audiences: [],
    goals: [],
    variations: [
      makeVariation(),
      makeVariation({
        id: OTHER_VARIATION_ID,
        key: `${OTHER_VARIATION_ID}-key`,
        name: 'Other Variation'
      })
    ],
    ...overrides
  } as unknown as ConfigExperience;
}

// The DataManager is always constructed with an EMPTY shared `experiences` list -- proving
// getPreviewDecision never depends on the experience being registered in shared config
// (mirrors a real `?exp=` scratch preview experience, which is not).
function makeDataManager(): InstanceType<typeof dm> {
  const config = {
    environment: 'production',
    data: {
      account_id: 'preview-decision-account',
      project: {id: 'preview-decision-project'},
      experiences: []
    }
  } as unknown as ConfigType;
  return new dm(config, {
    bucketingManager,
    ruleManager,
    eventManager,
    apiManager
  });
}

// Builds an experience carrying a "target" variation (subject to the case's overrides) plus
// an unrelated "other" variation, so tests can prove getPreviewDecision resolves exactly the
// requested id and nothing else.
function buildCaseExperience(
  experienceOverrides: Partial<ConfigExperience> = {},
  variationOverrides: Partial<ExperienceVariationConfig> = {}
): {experience: ConfigExperience; targetVariation: ExperienceVariationConfig} {
  const targetVariation = makeVariation(variationOverrides);
  const otherVariation = makeVariation({
    id: OTHER_VARIATION_ID,
    key: `${OTHER_VARIATION_ID}-key`,
    name: 'Other Variation'
  });
  const experience = makeExperience({
    ...experienceOverrides,
    variations: [targetVariation, otherVariation]
  });
  return {experience, targetVariation};
}

// Shared assertion: the returned decision is the REQUESTED variation, carrying the
// experience's id/key/name, and the call produced NO side effects (no apiManager.enqueue
// call, no new/changed entry in the in-memory visitor store).
function assertPreviewDecision(
  dataManager: InstanceType<typeof dm>,
  experience: ConfigExperience,
  requestedVariation: ExperienceVariationConfig,
  requestedVariationId: string
): BucketedVariation {
  const storeSizeBefore = (dataManager as any)._bucketedVisitors.size;
  const enqueueCountBefore = enqueueCallCount;

  const result = (dataManager as any).getPreviewDecision(
    experience,
    requestedVariationId
  );

  expect(result, 'getPreviewDecision result').to.be.an('object');
  expect((result as BucketedVariation).id).to.equal(requestedVariation.id);
  expect((result as BucketedVariation).key).to.equal(requestedVariation.key);
  expect((result as BucketedVariation).experienceId).to.equal(experience.id);
  expect((result as BucketedVariation).experienceKey).to.equal(
    experience.key
  );
  expect((result as BucketedVariation).experienceName).to.equal(
    experience.name
  );

  expect(enqueueCallCount, 'apiManager.enqueue call count').to.equal(
    enqueueCountBefore
  );
  expect(
    (dataManager as any)._bucketedVisitors.size,
    'in-memory visitor store size'
  ).to.equal(storeSizeBefore);

  return result as BucketedVariation;
}

describe('DataManager.getPreviewDecision() tests (SDK-4)', function () {
  beforeEach(function () {
    enqueueCallCount = 0;
  });

  // --- AC4: full bypass across draft/paused/environment-mismatch/non-running/zero-traffic ---
  describe('AC4 -- full bypass of status/environment/traffic filters', function () {
    interface BypassCase {
      name: string;
      experienceOverrides?: Partial<ConfigExperience>;
      variationOverrides?: Partial<ExperienceVariationConfig>;
    }

    const BYPASS_CASES: BypassCase[] = [
      {
        name: 'draft experience status is ignored',
        experienceOverrides: {status: ExperienceStatuses.DRAFT}
      },
      {
        name: 'paused experience status is ignored',
        experienceOverrides: {status: ExperienceStatuses.PAUSED}
      },
      {
        name: "mismatched experience environment ('staging' vs configured 'production') is ignored",
        experienceOverrides: {environment: 'staging'}
      },
      {
        name: 'non-RUNNING (stopped) variation status is ignored',
        variationOverrides: {status: VariationStatuses.STOPPED}
      },
      {
        name: 'traffic_allocation: 0 on the variation is ignored',
        variationOverrides: {traffic_allocation: 0}
      }
    ];

    // eslint-disable-next-line mocha/no-setup-in-describe
    BYPASS_CASES.forEach(({name, experienceOverrides, variationOverrides}) => {
      it(name, function () {
        const dataManager = makeDataManager();
        const {experience, targetVariation} = buildCaseExperience(
          experienceOverrides,
          variationOverrides
        );
        assertPreviewDecision(
          dataManager,
          experience,
          targetVariation,
          TARGET_VARIATION_ID
        );
      });
    });
  });

  // --- Stored-decision dimension: getPreviewDecision never reads the visitor store ---
  describe('AC4 -- stored bucketing decisions are ignored entirely', function () {
    it('returns the requested variation even when the visitor already has a stored decision for a different variation on this experience', function () {
      const dataManager = makeDataManager();
      const {experience, targetVariation} = buildCaseExperience();

      // Seed a stored decision that disagrees with the requested variation.
      dataManager.putData(PREVIEW_VISITOR_ID, {
        bucketing: {[experience.id.toString()]: OTHER_VARIATION_ID}
      });

      assertPreviewDecision(
        dataManager,
        experience,
        targetVariation,
        TARGET_VARIATION_ID
      );
    });
  });

  // --- AC7 (variation half): unknown variationId is inert ---
  describe('AC7 -- unknown variationId', function () {
    it('returns null when the requested variationId is not present on the experience', function () {
      const dataManager = makeDataManager();
      const {experience} = buildCaseExperience();

      const result = (dataManager as any).getPreviewDecision(
        experience,
        UNKNOWN_VARIATION_ID
      );

      expect(result).to.be.null;
    });
  });
});
