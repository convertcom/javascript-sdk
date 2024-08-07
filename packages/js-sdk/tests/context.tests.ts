/* eslint-disable mocha/consistent-spacing-between-blocks */
import 'mocha';
import {expect} from 'chai';
import {assert} from 'chai';
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
import {Config as ConfigType} from '@convertcom/js-sdk-types';
import {objectDeepMerge} from '@convertcom/js-sdk-utils';
import {EntityType} from '@convertcom/js-sdk-enums';
import {defaultConfig} from '../src/config/default';
import {
  getFeaturesWithStatuses,
  getMultipleFeatureWithStatus,
  getSingleFeatureWithStatus,
  getVariationsAcrossAllExperiences
} from './setup/shared';

const host = 'http://localhost';
const port = 8090;
const release_timeout = 1000;
const test_timeout = release_timeout + 1000;
const batch_size = 5;

const configuration = objectDeepMerge(testConfig, defaultConfig, {
  api: {
    endpoint: {
      config: host + ':' + port,
      track: host + ':' + port
    }
  },
  events: {
    batch_size: batch_size,
    release_interval: release_timeout
  }
}) as unknown as ConfigType;
const bucketingManager = new bm(configuration);
const ruleManager = new rm(configuration);
const eventManager = new em(configuration);
const apiManager = new am(configuration, {eventManager});

describe('Context tests', function () {
  const visitorId = 'XXX';
  const featureId = '10025';
  it('Should expose Context', function () {
    assert.isDefined(c);
  });
  it('Imported entity should be a constructor of Context instance', function () {
    expect(c)
      .to.be.a('function')
      .that.has.property('name')
      .which.equal('Context');
  });
  it('Should successfully create new Context instance', async function () {
    const dataManager = new dm(configuration, {
      bucketingManager,
      ruleManager,
      eventManager,
      apiManager
    });
    const experienceManager = new exm(configuration, {dataManager});
    const featureManager = new fm(configuration, {dataManager});
    const segmentsManager = new sm(configuration, {dataManager, ruleManager});
    const context = new c(configuration, visitorId, {
      eventManager,
      experienceManager,
      featureManager,
      segmentsManager,
      dataManager,
      apiManager
    });
    expect(context)
      .to.be.an('object')
      .that.has.property('constructor')
      .that.has.property('name')
      .which.equal('Context');
  });
  describe('Test Context', function () {
    const visitorId = 'XXX';
    let dataManager,
      experienceManager,
      featureManager,
      segmentsManager,
      context,
      accountId,
      projectId,
      server;
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    before(function () {
      accountId = configuration?.data?.account_id;
      projectId = configuration?.data?.project?.id;
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
        {browser: 'chrome', country: 'US'}
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
    it('Shoud successfully get variation from specific experience', function (done) {
      this.timeout(test_timeout);
      const experienceKey = 'test-experience-ab-fullstack-2';
      const variation = context.runExperience(experienceKey, {
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        }
      });
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', () => {
            expect(variation)
              .to.be.an('object')
              .that.have.keys(
                'experienceId',
                'experienceKey',
                'experienceName',
                'bucketingAllocation',
                'id',
                'key',
                'name',
                'status',
                'changes',
                'is_baseline',
                'traffic_allocation'
              );
            expect(variation.experienceKey).to.equal(experienceKey);
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end('{}');
      });
    });
    it('Shoud successfully get variations across all experiences', function (done) {
      this.timeout(test_timeout);
      getVariationsAcrossAllExperiences(
        {
          accountId,
          projectId,
          context,
          server
        },
        done
      );
    });
    it('Shoud successfully get a single feature and its status', function (done) {
      this.timeout(test_timeout);
      getSingleFeatureWithStatus(
        {
          accountId,
          projectId,
          featureId,
          context,
          server
        },
        done
      );
    });
    it('Shoud successfully get multiple features and its status', function (done) {
      this.timeout(test_timeout);
      getMultipleFeatureWithStatus(
        {
          accountId,
          projectId,
          context,
          server
        },
        done
      );
    });
    it('Shoud successfully get features and their statuses', function (done) {
      this.timeout(test_timeout);
      getFeaturesWithStatuses(
        {
          accountId,
          projectId,
          context,
          server
        },
        done
      );
    });
    it('Should trigger Conversion', function (done) {
      this.timeout(test_timeout);
      const goalKey = 'increase-engagement';
      const requestData = {
        source: 'js-sdk',
        enrichData: true,
        accountId,
        projectId,
        visitors: [
          {
            visitorId,
            events: [
              {
                eventType: 'conversion',
                data: {
                  goalId: '100215960'
                }
              },
              {
                eventType: 'conversion',
                data: {
                  goalId: '100215960',
                  goalData: [
                    {
                      key: 'amount',
                      value: 10.3
                    },
                    {
                      key: 'productsCount',
                      value: 2
                    }
                  ]
                }
              }
            ]
          }
        ]
      };
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          const body = [];
          request
            .on('data', (chunk) => {
              body.push(chunk);
            })
            .on('end', () => {
              const data = JSON.parse(Buffer.concat(body).toString());
              expect(data).to.be.an('object').that.deep.equal(requestData);
              done();
            });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end('{}');
      });
      context.trackConversion(goalKey, {
        ruleData: {
          action: 'buy'
        },
        conversionData: [
          {
            key: 'amount',
            value: 10.3
          },
          {
            key: 'productsCount',
            value: 2
          }
        ]
      });
    });
    it('Should fail to trigger Conversion if passing invalid goal data', function () {
      this.timeout(test_timeout);
      const goalKey = 'increase-engagement';
      const response = context.trackConversion(goalKey, {
        ruleData: {
          action: 'buy'
        },
        conversionData: [
          {
            key: 'amount',
            value: 10.3
          },
          {
            key: 'productsCount',
            value: 2
          }
        ]
      });
      expect(response).to.be.undefined;
    });
    it('Should successfully set default segments', function () {
      const segments = {country: 'UK'};
      context.setDefaultSegments(segments);
      const localSegments = dataManager.getData(visitorId);
      expect(segments).to.deep.equal(localSegments?.segments);
    });
    it('Should successfully run custom segments', function () {
      const segmentKey = 'test-segments-1';
      const segmentId = '200299434';
      context.runCustomSegments(segmentKey, {
        ruleData: {
          enabled: true
        }
      });
      const {segments} = dataManager.getData(visitorId) || {};
      expect(segments)
        .to.be.an('object')
        .that.has.property('customSegments')
        .to.deep.equal([segmentId]);
    });
    it('Should successfully update visitor properties', function () {
      const properties = {weather: 'rainy'};
      context.updateVisitorProperties(visitorId, properties);
      const localSegments = dataManager.getData(visitorId);
      expect(properties).to.deep.equal(localSegments?.segments);
    });
    it('Should successfully get config entity', function () {
      const audienceKey = 'adv-audience';
      const audienceEntity = context.getConfigEntity(
        audienceKey,
        EntityType.AUDIENCE
      );
      const audience = configuration?.data?.audiences?.find?.(
        ({key}) => key === audienceKey
      );
      expect(audienceEntity).to.deep.equal(audience);
      const segmentKey = 'test-segments-1';
      const segmentEntity = context.getConfigEntity(
        segmentKey,
        EntityType.SEGMENT
      );
      const segment = configuration?.data?.segments?.find?.(
        ({key}) => key === segmentKey
      );
      expect(segmentEntity).to.deep.equal(segment);
      const featureKey = 'feature-2';
      const featureEntity = context.getConfigEntity(
        featureKey,
        EntityType.FEATURE
      );
      const feature = configuration?.data?.features?.find?.(
        ({key}) => key === featureKey
      );
      expect(featureEntity).to.deep.equal(feature);
      const goalKey = 'adv-goal-country-browser';
      const goalEntity = context.getConfigEntity(goalKey, EntityType.GOAL);
      const goal = configuration?.data?.goals?.find?.(
        ({key}) => key === goalKey
      );
      expect(goalEntity).to.deep.equal(goal);
      const experienceKey = 'test-experience-ab-fullstack-3';
      const experienceEntity = context.getConfigEntity(
        experienceKey,
        EntityType.EXPERIENCE
      );
      const experience = configuration?.data?.experiences?.find?.(
        ({key}) => key === experienceKey
      );
      expect(experienceEntity).to.deep.equal(experience);
      const variationKey = '100299461-variation-1';
      const variationEntity = context.getConfigEntity(
        variationKey,
        EntityType.VARIATION
      );
      const variation = experience?.variations?.find?.(
        ({key}) => key === variationKey
      );
      expect(variationEntity).to.deep.equal(variation);
    });
    it('Should successfully get config entity by id', function () {
      const audienceId = '100299433';
      const audienceEntity = context.getConfigEntityById(
        audienceId,
        EntityType.AUDIENCE
      );
      const audience = configuration?.data?.audiences?.find?.(
        ({id}) => id === audienceId
      );
      expect(audienceEntity).to.deep.equal(audience);
      const segmentId = '200299434';
      const segmentEntity = context.getConfigEntityById(
        segmentId,
        EntityType.SEGMENT
      );
      const segment = configuration?.data?.segments?.find?.(
        ({id}) => id === segmentId
      );
      expect(segmentEntity).to.deep.equal(segment);
      const featureEntity = context.getConfigEntityById(
        featureId,
        EntityType.FEATURE
      );
      const feature = configuration?.data?.features?.find?.(
        ({id}) => id === featureId
      );
      expect(featureEntity).to.deep.equal(feature);
      const goalId = '100215961';
      const goalEntity = context.getConfigEntityById(goalId, EntityType.GOAL);
      const goal = configuration?.data?.goals?.find?.(({id}) => id === goalId);
      expect(goalEntity).to.deep.equal(goal);
      const experienceId = '100218246';
      const experienceEntity = context.getConfigEntityById(
        experienceId,
        EntityType.EXPERIENCE
      );
      const experience = configuration?.data?.experiences?.find?.(
        ({id}) => id === experienceId
      );
      expect(experienceEntity).to.deep.equal(experience);
      const variationId = '100299461';
      const variationEntity = context.getConfigEntityById(
        variationId,
        EntityType.VARIATION
      );
      const variation = experience?.variations?.find?.(
        ({id}) => id === variationId
      );
      expect(variationEntity).to.deep.equal(variation);
    });
    it('Should successfully get visitor data', function () {
      const experienceKey = 'test-experience-ab-fullstack-2';
      const variation = context.runExperience(experienceKey, {
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        }
      });
      const data = context.getVisitorData();
      expect(data).to.deep.equal({
        bucketing: {[variation.experienceId]: variation.id}
      });
    });
  });
  describe('Test invalid visitor', function () {
    let dataManager,
      experienceManager,
      featureManager,
      segmentsManager,
      context;
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
      context = new c(configuration, null, {
        eventManager,
        experienceManager,
        featureManager,
        segmentsManager,
        dataManager,
        apiManager
      });
    });
    it('Shoud fail to get variation from specific experience if no visitor is set', function () {
      const experienceKey = 'test-experience-ab-fullstack-2';
      const variation = context.runExperience(experienceKey);
      expect(variation).to.be.undefined;
    });
    it('Shoud fail to get variations across all experiences if no visitor is set', function () {
      const variations = context.runExperiences();
      expect(variations).to.be.undefined;
    });
    it('Shoud fail to get feature and its status if no visitor is set', function () {
      const featureKey = 'feature-1';
      const features = context.runFeature(featureKey);
      expect(features).to.be.undefined;
    });
    it('Shoud fail to get features and their statuses if no visitor is set', function () {
      const features = context.runFeatures();
      expect(features).to.be.undefined;
    });
    it('Should fail to trigger Conversion if no visitor is set', function () {
      const goalKey = 'increase-engagement';
      const output = context.trackConversion(goalKey);
      expect(output).to.be.undefined;
    });
    it('Should fail to set custom segments if no visitor is set', function () {
      const segmentKey = 'test-segments-1';
      const output = context.setCustomSegments(segmentKey);
      expect(output).to.be.undefined;
    });
  });
});
