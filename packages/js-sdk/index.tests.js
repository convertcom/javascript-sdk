import {expect} from 'chai';
import {assert} from 'chai';
import testConfig from './tests/test-config.json';

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
testConfig.dataStore = dataStore;

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
        'setCustomSegments'
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
      context = convert.createContext(visitorId, {
        browser: 'chrome'
      });
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
      const variationIds = [100299456, 100299457, 100299460, 100299461];
      const variations = context.runExperiences({
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        }
      });
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
    it('Shoud successfully get a single feature and its status', function (done) {
      const featureKey = 'feature-2';
      const featureId = 10025;
      const feature = context.runFeature(featureKey, {
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        }
      });
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
    it('Shoud successfully get multiple features and its status', function (done) {
      const featureKey = 'feature-1';
      const featureIds = [10024, 10025];
      const features = context.runFeature(featureKey, {
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        }
      });
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
    it('Shoud successfully get features and their statuses', function (done) {
      const featureIds = [10024, 10025, 10026];
      const features = context.runFeatures({
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        }
      });
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
    it('Should trigger Conversion', function () {
      const goalKey = 'increase-engagement';
      const response = context.trackConversion(goalKey, {
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
      expect(response).to.be.undefined;
    });
    it('Should successfully set default segments', function () {
      const segments = {country: 'US'};
      context.setDefaultSegments(segments);
      const localSegments = dataStore.get(storeKey);
      expect(localSegments)
        .to.have.property('segments')
        .that.deep.equal({...segments, ...defaultSegments});
    });
    it('Should successfully set custom segments', function () {
      const segmentKey = 'test-segments-1';
      const segmentId = '200299434';
      context.setCustomSegments(segmentKey, {
        ruleData: {
          enabled: true
        }
      });
      const {segments} = dataStore.get(storeKey) || {};
      expect(segments)
        .to.be.an('object')
        .that.has.property('customSegments')
        .to.deep.equal([segmentId]);
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
      const output = context.setCustomSegments(segmentKey);
      expect(output).to.be.undefined;
    });
  });
}
