/* eslint-disable mocha/consistent-spacing-between-blocks */
import 'mocha';
import {expect} from 'chai';
import {assert} from 'chai';
import http from 'http';

import {BucketingManager as bm} from '@convertcom/js-sdk-bucketing';
import {RuleManager as rm} from '@convertcom/js-sdk-rules';
import {BucketingError} from '@convertcom/js-sdk-enums';
import {EventManager as em} from '@convertcom/js-sdk-event';
import {ApiManager as am} from '@convertcom/js-sdk-api';
import {DataManager as dm} from '../src/data-manager';
import testConfig from './test-config.json';
import {Config as ConfigType} from '@convertcom/js-sdk-types';
import {objectDeepMerge} from '@convertcom/js-sdk-utils';
import {defaultConfig} from '../../js-sdk/src/config/default';

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
const host = 'http://localhost';
const port = 8090;
const release_timeout = 1000;
const test_timeout = release_timeout + 1000;
const batch_size = 10;

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

describe('DataManager tests', function () {
  const visitorId = 'XXX',
    bucketing = {
      exp1: 'var1',
      exp2: 'var2'
    },
    goals = {
      goal1: true,
      goal2: true
    },
    segments = {
      browser: 'CH',
      devices: 'ALLPH',
      source: 'test',
      campaign: 'test',
      visitorType: 'new',
      country: 'US',
      customSegments: ['seg1', 'seg2']
    };
  let dataManager, accountId, projectId, storeKey, server;
  // eslint-disable-next-line mocha/no-hooks-for-single-case
  before(function () {
    accountId = configuration?.data?.account_id;
    projectId = configuration?.data?.project?.id;
    storeKey = `${accountId}-${projectId}-${visitorId}`;
    dataManager = new dm(configuration, {
      bucketingManager,
      ruleManager,
      eventManager,
      apiManager
    });
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
  it('Should expose DataManager', function () {
    assert.isDefined(dm);
  });
  it('Imported entity should be a constructor of DataManager instance', function () {
    expect(dm)
      .to.be.a('function')
      .that.has.property('name')
      .which.equal('DataManager');
  });
  it('Should successfully create new DataManager instance', async function () {
    expect(dataManager)
      .to.be.an('object')
      .that.has.property('constructor')
      .that.has.property('name')
      .which.equal('DataManager');
  });
  describe('Test Data Manager', function () {
    it('Should successfully validate configuration', function () {
      const check = dataManager.isValidConfigData(configuration?.data);
      expect(check).to.equal(true);
    });
    it('Should retrieve variation for visitor by key', function (done) {
      this.timeout(test_timeout);
      const experienceKey = 'test-experience-ab-fullstack-2';
      const variation = dataManager.getBucketing(visitorId, experienceKey, {
        visitorProperties: {
          varName3: 'something'
        },
        locationProperties: {url: 'https://convert.com/'}
      });
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', function () {
            expect(variation)
              .to.be.an('object')
              .that.have.property('experienceKey');
            expect(variation.experienceKey).to.equal(experienceKey);
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end('{}');
      });
    });
    it('Should retrieve variation for visitor by id', function (done) {
      this.timeout(test_timeout);
      const experienceId = '100218245';
      const variation = dataManager.getBucketingById(visitorId, experienceId, {
        visitorProperties: {
          varName3: 'something'
        },
        locationProperties: {url: 'https://convert.com/'}
      });
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', function () {
            expect(variation)
              .to.be.an('object')
              .that.have.property('experienceId');
            expect(variation.experienceId).to.equal(experienceId);
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end('{}');
      });
    });
    it('Should get list of data entities grouped by field', function () {
      const audiences = dataManager.getEntitiesListObject('audiences');
      const {id} = configuration?.data?.audiences?.[0] || {};
      expect(audiences)
        .to.be.an('object')
        .that.deep.equal({
          [id]: configuration?.data?.audiences?.[0]
        });
    });
    it('Should find the entity in list by keys', function () {
      const keys = ['feature-1', 'feature-2'];
      const entityType = 'features';
      const entities = dataManager.getEntities(keys, entityType);
      expect(entities)
        .to.be.an('array')
        .that.deep.equal(
          configuration?.data?.[entityType]?.filter(({key}) =>
            keys.includes(key)
          )
        );
    });
    it('Should find the entity in list by ids', function () {
      const ids = ['10024', '10025'];
      const entityType = 'features';
      const entities = dataManager.getEntitiesByIds(ids, entityType);
      expect(entities)
        .to.be.an('array')
        .that.deep.equal(
          configuration?.data?.[entityType]?.filter(({id}) => ids.includes(id))
        );
    });
  });
  describe('Test requests enqueuing', function () {
    it('Should process conversion event', function (done) {
      this.timeout(test_timeout);
      const goalKey = 'increase-engagement';
      const requestData = {
        enrichData: true,
        source: 'js-sdk',
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
                      value: 10.4
                    },
                    {
                      key: 'productsCount',
                      value: 3
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
            .on('end', function () {
              const data = JSON.parse(Buffer.concat(body).toString());
              expect(data).to.be.an('object').that.deep.equal(requestData);
              done();
            });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end('{}');
      });
      dataManager.convert(
        visitorId,
        goalKey,
        {
          action: 'buy'
        },
        [
          {
            key: 'amount',
            value: 10.4
          },
          {
            key: 'productsCount',
            value: 3
          }
        ]
      );
    });
    it('Should fail if conversion event has an invalid goal', function () {
      this.timeout(test_timeout);
      const goalKey = 'invalid-goal';
      const response = dataManager.convert(visitorId, goalKey);
      expect(response).to.be.undefined;
    });
    it('Should fail if conversion event has a mismatched rule', function () {
      this.timeout(test_timeout);
      const goalKey = 'increase-engagement';
      const response = dataManager.convert(visitorId, goalKey, {
        action: 'sell'
      });
      expect(response).to.be.undefined;
    });
    it('Should fail if conversion event has an no rule', function () {
      this.timeout(test_timeout);
      const goalKey = 'goal-without-rule';
      const response = dataManager.convert(visitorId, goalKey, {
        action: 'buy'
      });
      expect(response).to.be.undefined;
    });
    it('Should fail to retrieve variation if not exists', function () {
      this.timeout(test_timeout);
      const experienceKey = 'test-experience-ab-fullstack-4';
      const variation = dataManager.getBucketing(visitorId, experienceKey, {
        visitorProperties: {
          varName3: 'something'
        },
        locationProperties: {url: 'https://convert.com/'}
      });
      expect(variation).to.be.equal(BucketingError.VARIAION_NOT_DECIDED);
    });
    it('Should never fail on reaching size limit when updating local store', function () {
      new Array(10001)
        .fill(0)
        .forEach((v, i) => dataManager.putData(`a${i}`, {test: i}));
      assert.equal(true, true);
    });
  });
  describe('Persistent Data Store enqueue tests', function () {
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    before(function () {
      configuration.dataStore = dataStore;
      dataManager = new dm(
        configuration,
        {
          bucketingManager,
          ruleManager,
          eventManager,
          apiManager
        },
        {
          asyncStorage: true
        }
      );
      // dataManager.dataStoreManager = dataStore;
    });
    it('Should successfully visitor bucketing (enqueuing)', function (done) {
      dataManager.putData(visitorId, {bucketing});
      setTimeout(function () {
        const check = dataManager.dataStoreManager.get(storeKey);
        expect(check).to.deep.equal({bucketing});
        done();
      }, release_timeout + 1);
    });
    it('Should successfully enqueue visitor goals', function (done) {
      dataManager.putData(visitorId, {goals});
      setTimeout(function () {
        const check = dataManager.dataStoreManager.get(storeKey);
        expect(check).to.deep.equal({bucketing, goals});
        done();
      }, release_timeout + 1);
    });
    it('Should successfully enqueue visitor segments', function (done) {
      dataManager.putData(visitorId, {segments});
      setTimeout(function () {
        const check = dataManager.dataStoreManager.get(storeKey);
        expect(check).to.deep.equal({bucketing, goals, segments});
        done();
      }, release_timeout + 1);
    });
    it('Should have the correct shape for visitor data', function () {
      const check = dataManager.dataStoreManager.get(storeKey);
      expect(check).to.be.an('object');
      expect(check).to.have.property('bucketing').that.deep.equal(bucketing);
      expect(check).to.have.property('goals').that.deep.equal(goals);
      expect(check).to.have.property('segments').that.deep.equal(segments);
    });
  });
  describe('Persistent Data Store tests (set immediately)', function () {
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    before(function () {
      dataStore.data = {};
      delete configuration.dataStore;
      dataManager = new dm(
        configuration,
        {
          bucketingManager,
          ruleManager,
          eventManager,
          apiManager
        },
        {
          asyncStorage: false
        }
      );
      dataManager.setDataStore(dataStore);
    });
    it('Should successfully set visitor bucketing', function () {
      dataManager.putData(visitorId, {bucketing});
      const check = dataManager.dataStoreManager.get(storeKey);
      expect(check).to.deep.equal({bucketing});
    });
    it('Should successfully set visitor goals', function () {
      dataManager.putData(visitorId, {goals});
      const check = dataManager.dataStoreManager.get(storeKey);
      expect(check).to.deep.equal({bucketing, goals});
    });
    it('Should successfully set visitor segments', function () {
      dataManager.putData(visitorId, {segments});
      const check = dataManager.dataStoreManager.get(storeKey);
      expect(check).to.deep.equal({bucketing, goals, segments});
    });
    it('Should have the correct shape for visitor data', function () {
      const check = dataManager.dataStoreManager.get(storeKey);
      expect(check).to.be.an('object');
      expect(check).to.have.property('bucketing').that.deep.equal(bucketing);
      expect(check).to.have.property('goals').that.deep.equal(goals);
      expect(check).to.have.property('segments').that.deep.equal(segments);
    });
  });
});
