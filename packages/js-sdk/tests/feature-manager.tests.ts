import 'mocha';
import {expect} from 'chai';
import {assert} from 'chai';
import http from 'http';

import {BucketingManager as bm} from '@convertcom/bucketing';
import {RuleManager as rm} from '@convertcom/rules';
import {EventManager as em} from '@convertcom/event';
import {ApiManager as am} from '../src/api-manager';
import {DataManager as dm} from '../src/data-manager';
import {FeatureManager as fm} from '../src/feature-manager';
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

describe('FeatureManager tests', function () {
  const visitorId = 'XXX';
  let dataManager, featureManager, accountId, projectId, server;
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
    featureManager = new fm(configuration, {dataManager});
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
  it('Should expose FeatureManager', function () {
    assert.isDefined(fm);
  });
  it('Imported entity should be a constructor of FeatureManager instance', function () {
    expect(fm)
      .to.be.a('function')
      .that.has.property('name')
      .which.equal('FeatureManager');
  });
  it('Should successfully create new FeatureManager instance', async function () {
    expect(featureManager)
      .to.be.an('object')
      .that.has.property('constructor')
      .that.has.property('name')
      .which.equal('FeatureManager');
  });

  describe('Test Feature Manager', function () {
    it('Shoud successfully get a list of all entities', function () {
      const entities = featureManager.getList();
      expect(entities)
        .to.be.an('array')
        .that.has.length(3)
        .to.deep.equal(configuration?.data?.features);
    });
    it('Shoud successfully get a list of all entities as object of entities grouped by identity field', function () {
      const field = 'id';
      const entities = featureManager.getListAsObject(field);
      const featuresList = Object.fromEntries(
        configuration?.data?.features?.map((feature) => [
          feature?.[field],
          feature
        ]) || []
      );
      expect(entities).to.be.an('object').that.deep.equal(featuresList);
    });
    it('Shoud successfully get the entity by key', function () {
      const featureKey = 'feature-1';
      const featureId = 10024;
      const entity = featureManager.getFeature(featureKey);
      expect(entity)
        .to.be.an('object')
        .that.has.property('id')
        .to.equal(featureId);
    });
    it('Shoud successfully get the entity by id', function () {
      const featureKey = 'feature-1';
      const featureId = 10024;
      const entity = featureManager.getFeatureById(featureId);
      expect(entity)
        .to.be.an('object')
        .that.has.property('key')
        .to.equal(featureKey);
    });
    it('Shoud successfully specific entities by array of keys', function () {
      const featureKeys = ['feature-1', 'feature-2', 'not-attached-feature-3'];
      const entities = featureManager.getFeatures(featureKeys);
      expect(entities)
        .to.be.an('array')
        .to.deep.equal(configuration?.data?.features);
    });
    it('Shoud successfully get a specific variable type defined in a specific feature', function () {
      const featureKey = 'feature-1';
      const variableName = 'enabled';
      const variableType = 'boolean';
      const type = featureManager.getFeatureVariableType(
        featureKey,
        variableName
      );
      expect(type).to.equal(variableType);
    });
    it('Shoud successfully get specific variable type defined in a specific feature by id', function () {
      const featureId = 10024;
      const variableName = 'enabled';
      const variableType = 'boolean';
      const type = featureManager.getFeatureVariableTypeById(
        featureId,
        variableName
      );
      expect(type).to.equal(variableType);
    });
    it('Shoud successfully check that feature is declared', function () {
      const featureKey = 'feature-1';
      const check = featureManager.isFeatureDeclared(featureKey);
      expect(check).to.equal(true);
    });
    it('Shoud successfully get feature and its status', function (done) {
      this.timeout(test_timeout);
      const featureKey = 'feature-1';
      const featureIds = [10024, 10025];
      const features = featureManager.runFeature(
        visitorId,
        featureKey,
        {
          varName3: 'something'
        },
        {url: 'https://convert.com/'}
      );
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', () => {
            expect(features).to.be.an('array').that.have.length(2);
            const selectedFeatures = features.map(({id}) => id);
            expect(featureIds).to.include.deep.members(selectedFeatures);
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end();
      });
    });
    it('Shoud successfully check is feature enabled', function (done) {
      this.timeout(test_timeout);
      const featureKey = 'feature-1';
      const enabled = featureManager.isFeatureEnabled(
        visitorId,
        featureKey,
        {
          varName3: 'something'
        },
        {url: 'https://convert.com/'}
      );
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', () => {
            expect(enabled).to.equal(true);
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end();
      });
    });
    it('Shoud successfully get feature and its status by id', function (done) {
      this.timeout(test_timeout);
      const featureId = 10024;
      const featureIds = [10024, 10025];
      const features = featureManager.runFeatureById(
        visitorId,
        featureId,
        {
          varName3: 'something'
        },
        {url: 'https://convert.com/'}
      );
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', () => {
            expect(features).to.be.an('array').that.have.length(2);
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
      const filterByFeatures = [
        'feature-1',
        'feature-2',
        'not-attached-feature-3'
      ];
      const filterByExperiences = [
        'test-experience-ab-fullstack-2',
        'test-experience-ab-fullstack-3'
      ];
      const featureIds = [10024, 10025, 10026];
      const features = featureManager.runFeatures(
        visitorId,
        {
          varName3: 'something'
        },
        {url: 'https://convert.com/'},
        true,
        {
          features: filterByFeatures,
          experiences: filterByExperiences
        }
      );
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', () => {
            expect(features).to.be.an('array').that.have.length(3);
            const selectedFeatures = features.map(({id}) => id);
            expect(featureIds).to.include.deep.members(selectedFeatures);
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end();
      });
    });
    it('Convert value type', function () {
      let value = featureManager.castType('123', 'integer');
      expect(typeof value).to.equal('number');
      value = featureManager.castType(123, 'string');
      expect(typeof value).to.equal('string');
      value = featureManager.castType('1.23', 'float');
      expect(typeof value).to.equal('number');
      value = featureManager.castType('false', 'boolean');
      expect(typeof value).to.equal('boolean');
    });
  });
});
