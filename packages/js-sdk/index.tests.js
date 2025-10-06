/* eslint-disable mocha/consistent-spacing-between-blocks */
import {expect} from 'chai';
import {assert} from 'chai';
import testConfig from './tests/test-config.json';
import {
  getFeaturesWithStatuses,
  getMultipleFeatureWithStatus,
  getSingleFeatureWithStatus,
  getVariationsAcrossAllExperiences
} from './tests/setup/shared';

class DataStore {
  data = {};
  get(key) {
    if (!key) return this.data;
    return this.data[key.toString()];
  }
  set(key, value) {
    if (!key) throw new Error('Invalid DataStore key!');
    this.data[key.toString()] = value;
  }
}

const dataStore = new DataStore();
testConfig.experimental = {
  useCoreDecider: true
};
testConfig.dataStore = dataStore;
testConfig.events = {
  batch_size: 1,
  release_interval: 1000
};

const accountId = testConfig.data.account_id;
const projectId = testConfig.data.project.id;
const visitorId = 'XXX';
const storeKey = `${accountId}-${projectId}-${visitorId}`;

const defaultSegments = {browser: 'chrome'};

// eslint-disable-next-line mocha/no-exports
export default function runTests(bundle) {
  const ConvertSDK = bundle.default;
  let convert, context;
  describe('Basic SDK instance', function () {
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    beforeEach(function () {
      convert = new ConvertSDK(testConfig);
    });
    it('Should have an SDK instance as an object', function () {
      expect(bundle).to.be.an('object');
    });
    it('Should have a constructor', function () {
      expect(ConvertSDK).to.be.a('function');
    });
    it('Should create a default SDK instance and fire ready event. Expect no errors', function (done) {
      convert.on('ready', function (args, err) {
        expect(err).to.be.null;
        //expect(convert).to.have.property('version').which.is.a('string');
        expect(convert).to.be.an('object');
        done();
      });
    });
    it('Should create a default SDK instance and resolve a promise. Expect no errors', async function () {
      await convert.onReady();
      assert.equal(true, true);
    });
    it('Shoud successfully create visitor context', function () {
      const visitorContext = convert.createContext(visitorId, defaultSegments);
      expect(visitorContext).to.be.an('object');
      [
        'runExperience',
        'runExperiences',
        'runFeature',
        'runFeatures',
        'trackConversion',
        'setDefaultSegments',
        'runCustomSegments'
      ].forEach((method) => {
        expect(visitorContext).to.have.a.property(method);
      });
    });
  });
  describe('Basic SDK methods', function () {
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    before(function () {
      convert = new ConvertSDK(testConfig);
    });
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    beforeEach(function () {
      context = convert.createContext(visitorId, defaultSegments);
    });
    it('Shoud successfully get variation from specific experience', function (done) {
      const experienceKey = 'test-experience-ab-fullstack-2';
      const variation = context.runExperience(experienceKey, {
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        }
      });
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
    it('Shoud successfully get variations across all experiences', function (done) {
      getVariationsAcrossAllExperiences(
        {
          accountId,
          projectId,
          context
        },
        done
      );
    });
    it('Shoud successfully get a single feature and its status', function (done) {
      const featureId = '10025';
      getSingleFeatureWithStatus(
        {
          accountId,
          projectId,
          featureId,
          context
        },
        done
      );
    });
    it('Shoud successfully get multiple features and its status', function (done) {
      getMultipleFeatureWithStatus(
        {
          accountId,
          projectId,
          context
        },
        done
      );
    });
    it('Shoud successfully get features and their statuses', function (done) {
      getFeaturesWithStatuses(
        {
          accountId,
          projectId,
          context
        },
        done
      );
    });
    it('Should trigger Conversion', function () {
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
    it('Should successfully set default segments', function (done) {
      const segments = {country: 'US'};
      context.setDefaultSegments(segments);
      setTimeout(function () {
        const localSegments = dataStore.get(storeKey);
        expect(localSegments)
          .to.have.property('segments')
          .that.deep.equal({
            ...segments,
            ...defaultSegments
          });
        done();
      }, testConfig.events.release_interval + 1);
    });
    it('Should successfully set custom segments', function (done) {
      const segmentKey = 'test-segments-1';
      const segmentId = '200299434';
      context.runCustomSegments(segmentKey, {
        ruleData: {
          enabled: true
        }
      });
      setTimeout(function () {
        const {segments} = dataStore.get(storeKey) || {};
        expect(segments)
          .to.be.an('object')
          .that.has.property('customSegments')
          .to.deep.equal([segmentId]);
        done();
      }, testConfig.events.release_interval + 1);
    });
  });
  describe('Test invalid visitor', function () {
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    before(function () {
      convert = new ConvertSDK(testConfig);
    });
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    beforeEach(function () {
      context = convert.createContext();
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
      const output = context.runCustomSegments(segmentKey);
      expect(output).to.be.undefined;
    });
  });
}
