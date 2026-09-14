/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

// CAP-1 (SPEC-per-call-bucketing-attributes): forwarding asserted at the
// DataManager.getBucketing() engine boundary, never via a FeatureManager
// spy.
import 'mocha';
import {expect} from 'chai';
import http from 'http';

import {BucketingManager as bm} from '@convertcom/js-sdk-bucketing';
import {RuleManager as rm} from '@convertcom/js-sdk-rules';
import {EventManager as em} from '@convertcom/js-sdk-event';
import {ApiManager as am} from '@convertcom/js-sdk-api';
import {DataManager as dm} from '@convertcom/js-sdk-data';
import {ExperienceManager as exm} from '@convertcom/js-sdk-experience';
import {FeatureManager as fm} from '../src/feature-manager';
import {SegmentsManager as sm} from '@convertcom/js-sdk-segments';
import {Context as c} from '../src/context';
import testConfig from './test-config.json';
import {Config as ConfigType, BucketingAttributes} from '@convertcom/js-sdk-types';
import {objectDeepMerge} from '@convertcom/js-sdk-utils';
import {FeatureStatus} from '@convertcom/js-sdk-enums';
import {defaultConfig} from '../src/config/default';

const host = 'http://localhost';
const port = 8093;

const configuration = objectDeepMerge(testConfig, defaultConfig, {
  api: {
    endpoint: {
      config: host + ':' + port,
      track: host + ':' + port
    }
  }
}) as unknown as ConfigType;
const bucketingManager = new bm(configuration);
const ruleManager = new rm(configuration);
const eventManager = new em(configuration);
const apiManager = new am(configuration, {eventManager});

const experienceKey = 'test-experience-ab-fullstack-2';
const featureKey = 'feature-2';
// Non-`SegmentsKeys` fields, so the constructor keeps them on
// `Context._visitorProperties` directly instead of routing them into
// `segmentsManager`-backed storage (which `dataManager.reset()` wipes
// between tests, unlike the Context instance's own in-memory field).
const constructorVisitorProperties = {persona: 'vip', tier: 'gold'};

// Records `DataManager.getBucketing()` calls (CAP-1's engine boundary).
function spyGetBucketing(dataManager: any): {
  calls: Array<{identity: string; attributes: BucketingAttributes}>;
  restore: () => void;
} {
  const original = dataManager.getBucketing.bind(dataManager);
  const calls: Array<{identity: string; attributes: BucketingAttributes}> = [];
  dataManager.getBucketing = function (
    visitorId: string,
    identity: string,
    attributes: BucketingAttributes
  ) {
    calls.push({identity, attributes});
    return original(visitorId, identity, attributes);
  };
  return {
    calls,
    restore: () => {
      dataManager.getBucketing = original;
    }
  };
}

describe('Context bucketing-attributes forwarding (engine boundary)', function () {
  const visitorId = 'ZZZ';
  let dataManager, experienceManager, featureManager, segmentsManager, context, server;

  // eslint-disable-next-line mocha/no-hooks-for-single-case
  before(function () {
    dataManager = new dm(configuration, {
      bucketingManager,
      ruleManager,
      eventManager,
      apiManager
    });
    experienceManager = new exm(configuration, {dataManager});
    featureManager = new fm(configuration, {dataManager});
    segmentsManager = new sm(configuration, {dataManager, ruleManager});
    context = new c(
      configuration,
      visitorId,
      {
        eventManager,
        experienceManager,
        featureManager,
        segmentsManager,
        dataManager,
        apiManager
      },
      constructorVisitorProperties
    );
  });
  // eslint-disable-next-line mocha/no-hooks-for-single-case
  beforeEach(function () {
    server = http.createServer();
    server.listen(port);
  });
  // eslint-disable-next-line mocha/no-hooks-for-single-case
  afterEach(function () {
    dataManager.reset();
    server.closeAllConnections();
    server.close();
  });

  const entryPoints: Array<{
    name: string;
    call: (ctx: any, attrs: BucketingAttributes) => any;
  }> = [
    {
      name: 'runExperience',
      call: (ctx, attrs) => ctx.runExperience(experienceKey, attrs)
    },
    {name: 'runExperiences', call: (ctx, attrs) => ctx.runExperiences(attrs)},
    {name: 'runFeature', call: (ctx, attrs) => ctx.runFeature(featureKey, attrs)},
    {name: 'runFeatures', call: (ctx, attrs) => ctx.runFeatures(attrs)}
  ];

  function forwardedFor(spy: ReturnType<typeof spyGetBucketing>): BucketingAttributes {
    const entry = spy.calls.find((call) => call.identity === experienceKey);
    expect(entry, `getBucketing() call for ${experienceKey}`).to.exist;
    return entry.attributes;
  }

  describe('every BucketingAttributes control field reaches DataManager.getBucketing()', function () {
    entryPoints.forEach(({name, call}) => {
      it(`forwards enableTracking, forceVariationId, ignoreLocationProperties, enableStorage, suppressEvents, updateVisitorProperties and locationProperties through ${name}`, function () {
        const spy = spyGetBucketing(dataManager);
        try {
          call(context, {
            enableTracking: false,
            forceVariationId: '100299461',
            ignoreLocationProperties: true,
            enableStorage: false,
            suppressEvents: true,
            updateVisitorProperties: true,
            visitorProperties: {varName3: 'something'},
            locationProperties: {url: 'https://convert.com/'}
          });
          const forwarded = forwardedFor(spy);
          expect(forwarded).to.include({
            enableTracking: false,
            forceVariationId: '100299461',
            ignoreLocationProperties: true,
            enableStorage: false,
            suppressEvents: true,
            updateVisitorProperties: true
          });
          expect(forwarded.locationProperties).to.deep.equal({
            url: 'https://convert.com/'
          });
        } finally {
          spy.restore();
        }
      });
    });
  });

  describe("Context-computed transforms still override the caller's raw field afterwards", function () {
    entryPoints.forEach(({name, call}) => {
      it(`${name} merges the per-call visitorProperties into the Context's own, rather than forwarding the per-call value alone`, function () {
        const spy = spyGetBucketing(dataManager);
        try {
          call(context, {
            visitorProperties: {varName3: 'something'},
            locationProperties: {url: 'https://convert.com/'}
          });
          const expected = objectDeepMerge(constructorVisitorProperties, {
            varName3: 'something'
          });
          expect(forwardedFor(spy).visitorProperties).to.deep.equal(expected);
        } finally {
          spy.restore();
        }
      });

      it(`${name} falls back to the Context's own environment when the caller supplies none`, function () {
        const spy = spyGetBucketing(dataManager);
        try {
          call(context, {
            visitorProperties: {varName3: 'something'},
            locationProperties: {url: 'https://convert.com/'}
          });
          expect(forwardedFor(spy).environment).to.equal('staging');
        } finally {
          spy.restore();
        }
      });
    });
  });

  describe('typeCasting defaults to true when the key is absent, and honours an explicit false (feature entry points only)', function () {
    entryPoints
      .filter(({name}) => name === 'runFeature' || name === 'runFeatures')
      .forEach(({name, call}) => {
        it(`${name} defaults typeCasting to true when absent`, function () {
          const spy = spyGetBucketing(dataManager);
          try {
            call(context, {
              visitorProperties: {varName3: 'something'},
              locationProperties: {url: 'https://convert.com/'}
            });
            expect(forwardedFor(spy).typeCasting).to.equal(true);
          } finally {
            spy.restore();
          }
        });

        it(`${name} honours an explicit typeCasting:false rather than defaulting it`, function () {
          const spy = spyGetBucketing(dataManager);
          try {
            call(context, {
              typeCasting: false,
              visitorProperties: {varName3: 'something'},
              locationProperties: {url: 'https://convert.com/'}
            });
            expect(forwardedFor(spy).typeCasting).to.equal(false);
          } finally {
            spy.restore();
          }
        });
      });
  });

  describe('ignoreLocationProperties is a behavioural gate: only a strict boolean true bypasses it', function () {
    const cases: Array<{
      label: string;
      value: any;
      expectDisabled: boolean;
    }> = [
      {label: 'boolean true', value: true, expectDisabled: false},
      {label: "string 'true'", value: 'true', expectDisabled: true},
      {label: 'number 1', value: 1, expectDisabled: true},
      {label: "string 'yes'", value: 'yes', expectDisabled: true}
    ];

    cases.forEach(({label, value, expectDisabled}) => {
      it(`runFeature with ignoreLocationProperties=${label} and no locationProperties ${
        expectDisabled ? 'still reports the feature disabled' : 'resolves the feature'
      }`, function () {
        const feature: any = context.runFeature(featureKey, {
          ignoreLocationProperties: value,
          visitorProperties: {varName3: 'something'}
        });
        if (expectDisabled) {
          expect(feature.status).to.equal(FeatureStatus.DISABLED);
        } else {
          expect(feature.status).to.not.equal(FeatureStatus.DISABLED);
        }
      });

      it(`runFeatures with ignoreLocationProperties=${label} and no locationProperties ${
        expectDisabled
          ? 'still reports feature-2 disabled'
          : 'resolves feature-2'
      }`, function () {
        const features: any = context.runFeatures({
          ignoreLocationProperties: value,
          visitorProperties: {varName3: 'something'}
        });
        const feature = features.find((f: any) => f.key === featureKey);
        if (expectDisabled) {
          expect(feature.status).to.equal(FeatureStatus.DISABLED);
        } else {
          expect(feature.status).to.not.equal(FeatureStatus.DISABLED);
        }
      });
    });
  });
});
