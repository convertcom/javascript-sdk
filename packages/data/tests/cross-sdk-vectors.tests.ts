/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * qs-01 (GOLD-1) — cross-SDK golden-vector bucketing contract runner.
 *
 * Spec of record: _bmad-output/planning-artifacts/2026-07-02-convert-js-sdk/qs-01-anchored-bucketing-layout.md
 * "Golden-vector fixture" section, AC2/AC3/AC4/AC5/AC6/AC7.
 *
 * This is the canonical cross-SDK parity fixture's JS reference runner. The fixture
 * (packages/bucketing/tests/cross-sdk-bucketing-vectors.json) is authored and owned by the
 * JS SDK (this repo, the reference implementation per qs-01) and is copied verbatim by the
 * five sibling SDK repos (PHP/Python/Ruby/Android/iOS) for their own parity suites — every
 * `expected` value below was computed by THIS implementation and then frozen; changing this
 * fixture is a cross-SDK contract change, not a local test tweak.
 *
 * Home decision: this runner lives in packages/data/tests (not packages/bucketing/tests)
 * because exercising the version gate end-to-end requires DataManager, and the workspace's
 * dependency direction only goes packages/data -> packages/bucketing (bucketing has no
 * dependency on data; see packages/bucketing/package.json's peerDependencies and the root
 * build order enums->types->utils->event->bucketing->...->data->...). The fixture asset
 * itself stays in packages/bucketing/tests per this feature's earlier instruction (that is
 * where the algorithm-level anchored tests already live); this file reads it via a relative
 * filesystem path (not a package import), so no new package dependency edge is introduced
 * and the existing build order is untouched. See this feature's decision log for the full
 * reasoning.
 *
 * Each vector is driven through DataManager.getBucketingById -- the exact same public seam
 * DATA-1's gate tests use -- so every vector exercises the REAL gate
 * (`Number(experience.version) > 11`), the real allocation-build mapping, and the real
 * bucketing managers, not a re-implementation of the algorithm.
 */
import 'mocha';
import {expect} from 'chai';
import * as fs from 'fs';
import * as path from 'path';
import {BucketingManager as bm} from '@convertcom/js-sdk-bucketing';
import {RuleManager as rm} from '@convertcom/js-sdk-rules';
import {EventManager as em} from '@convertcom/js-sdk-event';
import {ApiManager as am} from '@convertcom/js-sdk-api';
import {DataManager as dm} from '../src/data-manager';
import testConfig from './test-config.json';
import {
  Config as ConfigType,
  ConfigExperience,
  BucketingAttributes
} from '@convertcom/js-sdk-types';
import {objectDeepMerge} from '@convertcom/js-sdk-utils';
import {defaultConfig} from '../../js-sdk/src/config/default';
import {BucketingError} from '@convertcom/js-sdk-enums';

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

// Hoisted once: shared dependency managers + the fixture itself, per this repo's
// SonarCloud new_duplicated_lines_density <= 3% rule (no per-case setup/teardown).
const FIXTURE_PATH = path.resolve(
  __dirname,
  '../../bucketing/tests/cross-sdk-bucketing-vectors.json'
);
const VECTORS: CrossSdkVector[] = JSON.parse(
  fs.readFileSync(FIXTURE_PATH, 'utf8')
);

const configuration = objectDeepMerge(
  testConfig,
  defaultConfig,
  {}
) as unknown as ConfigType;
const bucketingManager = new bm(configuration);
const ruleManager = new rm(configuration);
const eventManager = new em(configuration);
const apiManager = new am(configuration, {eventManager});

// Every vector targets the fresh-bucketing GATE only, matching DATA-1's own gate-test
// attributes: `ignoreLocationProperties` bypasses site_area/locations entirely, an empty
// `visitorProperties` plus an empty `experience.audiences` list satisfies the "unrestricted"
// rule-matching branches, and `enableTracking: false` avoids depending on a live
// track-endpoint server.
const ATTRS: BucketingAttributes = {
  visitorProperties: {},
  ignoreLocationProperties: true,
  enableTracking: false,
  updateVisitorProperties: false
};

function runVector(vector: CrossSdkVector) {
  const experience = {
    id: vector.experienceId,
    name: `exp-${vector.experienceId}`,
    key: `key-${vector.experienceId}`,
    type: 'a/b_fullstack',
    audiences: [],
    goals: [],
    variations: vector.variations,
    version: vector.version
  } as unknown as ConfigExperience;

  const dataManager = new dm(
    {
      data: {
        account_id: 'cross-sdk-vectors-account',
        project: {id: 'cross-sdk-vectors-project'},
        experiences: [experience]
      }
    } as unknown as ConfigType,
    {bucketingManager, ruleManager, eventManager, apiManager}
  );

  return dataManager.getBucketingById(
    vector.visitorId,
    vector.experienceId,
    ATTRS
  );
}

describe('Cross-SDK golden-vector bucketing contract (qs-01 / GOLD-1, AC7)', function () {
  // eslint-disable-next-line mocha/no-setup-in-describe
  VECTORS.forEach((vector) => {
    it(vector.description, function () {
      const result = runVector(vector);
      if (vector.expected === null) {
        expect(result).to.equal(BucketingError.VARIAION_NOT_DECIDED);
      } else {
        expect(result)
          .to.be.an('object')
          .that.has.property('id', vector.expected);
      }
    });
  });

  it('loaded every required golden-vector category (AC7 completeness guard)', function () {
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
      expect(descriptions).to.include(tag);
    });
    expect(VECTORS.length).to.be.greaterThan(0);
  });
});
