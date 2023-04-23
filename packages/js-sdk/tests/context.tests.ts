import 'mocha';
import {expect} from 'chai';
import {assert} from 'chai';
import http from 'http';

import {BucketingManager as bm} from '@convertcom/bucketing';
import {RuleManager as rm} from '@convertcom/rules';
import {EventManager as em} from '../src/event-manager';
import {ApiManager as am} from '../src/api-manager';
import {DataManager as dm} from '../src/data-manager';
import {ExperienceManager as exm} from '../src/experience-manager';
import {FeatureManager as fm} from '../src/feature-manager';
import {SegmentsManager as sm} from '../src/segments-manager';
import {Context as c} from '../src/context';
import testConfig from './test-config.json';
import {Config} from '../src/config';

const host = 'http://localhost';
const port = 8090;
const release_timeout = 1000;
const test_timeout = release_timeout + 1000;
const batch_size = 5;

const configuration = Config({
  ...testConfig,
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
});
const bucketingManager = new bm(configuration);
const ruleManager = new rm(configuration);
const eventManager = new em(configuration);
const apiManager = new am(configuration, {eventManager});

describe('Context tests', function () {
  const visitorId = 'XXX';
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
      dataManager
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
          dataManager
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
      dataManager.putLocalStore(visitorId, {});
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
        res.end();
      });
    });
    it('Shoud successfully get variations across all experiences', function (done) {
      this.timeout(test_timeout);
      const variationIds = [100299456, 100299457, 100299460, 100299461];
      const variations = context.runExperiences({
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        }
      });
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', () => {
            expect(variations).to.be.an('array').that.have.length(2);
            variations.forEach((variation) =>
              expect(variation)
                .to.be.an('object')
                .that.have.keys(
                  'experienceId',
                  'experienceKey',
                  'experienceName',
                  'id',
                  'key',
                  'name',
                  'status',
                  'changes',
                  'is_baseline',
                  'traffic_allocation'
                )
            );
            const selectedVariations = variations.map(({id}) => id);
            expect(variationIds).to.include.deep.members(selectedVariations);
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end();
      });
    });
    it('Shoud successfully get a single feature and its status', function (done) {
      this.timeout(test_timeout);
      const featureKey = 'feature-2';
      const featureId = 10025;
      const feature = context.runFeature(featureKey, {
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        }
      });
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', () => {
            expect(feature)
              .to.be.an('object')
              .that.have.keys(
                'experienceId',
                'experienceKey',
                'experienceName',
                'id',
                'key',
                'name',
                'status',
                'variables'
              );
            expect(feature.id).to.equal(featureId);
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end();
      });
    });
    it('Shoud successfully get multiple features and its status', function (done) {
      this.timeout(test_timeout);
      const featureKey = 'feature-1';
      const featureIds = [10024, 10025];
      const features = context.runFeature(featureKey, {
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        }
      });
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', () => {
            expect(features).to.be.an('array').that.have.length(2);
            features.forEach((feature) =>
              expect(feature)
                .to.be.an('object')
                .that.have.keys(
                  'experienceId',
                  'experienceKey',
                  'experienceName',
                  'id',
                  'key',
                  'name',
                  'status',
                  'variables'
                )
            );
            const selectedFeatures = features.map(({id}) => id);
            expect(featureIds).to.include.deep.members(selectedFeatures);
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end();
      });
    });
    it('Shoud successfully get features and their statuses', function (done) {
      this.timeout(test_timeout);
      const featureIds = [10024, 10025, 10026];
      const features = context.runFeatures({
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        }
      });
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', () => {
            expect(features).to.be.an('array').that.have.length(4);
            features
              .filter(({status}) => status === 'enabled')
              .forEach((feature) =>
                expect(feature)
                  .to.be.an('object')
                  .that.have.keys(
                    'experienceId',
                    'experienceKey',
                    'experienceName',
                    'id',
                    'key',
                    'name',
                    'status',
                    'variables'
                  )
              );
            features
              .filter(({status}) => status === 'disabled')
              .forEach((feature) =>
                expect(feature)
                  .to.be.an('object')
                  .that.have.keys('id', 'key', 'name', 'status')
              );
            const selectedFeatures = features.map(({id}) => id);
            expect(featureIds).to.include.deep.members(selectedFeatures);
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end();
      });
    });
    it('Should trigger Conversion', function (done) {
      this.timeout(test_timeout);
      const goalKey = 'increase-engagement';
      const requestData = {
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
                  goalId: 100215960
                }
              },
              {
                eventType: 'conversion',
                data: {
                  goalId: 100215960,
                  goalData: [
                    {
                      amount: 10.3,
                      productsCount: 2
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
        res.end();
      });
      context.trackConversion(goalKey, {
        ruleData: {
          action: 'buy'
        },
        conversionData: [
          {
            amount: 10.3,
            productsCount: 2
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
        conversionData: {
          amount: 10.3,
          productsCount: 2
        }
      });
      expect(response).to.be.undefined;
    });
    it('Should successfully set default segments', function () {
      const segments = {country: 'US'};
      context.setDefaultSegments(segments);
      const localSegments = dataManager.getLocalStore(visitorId);
      expect(segments).to.deep.equal(localSegments?.segments);
    });
    it('Should successfully set custom segments', function () {
      const segmentKey = 'test-segments-1';
      const segmentId = '200299434';
      context.setCustomSegments(segmentKey, {
        ruleData: {
          enabled: true
        }
      });
      const {segments} = dataManager.getLocalStore(visitorId) || {};
      expect(segments)
        .to.be.an('object')
        .that.has.property('customSegments')
        .to.deep.equal([segmentId]);
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
        dataManager
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
