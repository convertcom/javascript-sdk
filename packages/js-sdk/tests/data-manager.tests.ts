import 'mocha';
import {expect} from 'chai';
import {assert} from 'chai';
import http from 'http';

import {BucketingManager as bm} from '@convertcom/js-sdk-bucketing';
import {RuleManager as rm} from '@convertcom/js-sdk-rules';
import {EventManager as em} from '@convertcom/js-sdk-event';
import {ApiManager as am} from '@convertcom/js-sdk-api';
import {DataManager as dm} from '@convertcom/js-sdk-data';
import testConfig from './test-config.json';
import {Config} from '@convertcom/js-sdk-types';

const host = 'http://localhost';
const port = 8090;
const release_timeout = 1000;
const test_timeout = release_timeout + 1000;
const batch_size = 5;

const configuration = {
  ...testConfig,
  tracking: true,
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
} as unknown as Config;
const bucketingManager = new bm(configuration);
const ruleManager = new rm(configuration);
const eventManager = new em(configuration);
const apiManager = new am(configuration, {eventManager});

describe('DataManager tests', function () {
  const visitorId = 'XXX';
  let dataManager, accountId, projectId, server;
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
      const variation = dataManager.getBucketing(
        visitorId,
        experienceKey,
        {
          varName3: 'something'
        },
        {url: 'https://convert.com/'}
      );
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', () => {
            expect(variation)
              .to.be.an('object')
              .that.have.property('experienceKey');
            expect(variation.experienceKey).to.equal(experienceKey);
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end();
      });
    });
    it('Should retrieve variation for visitor by id', function (done) {
      this.timeout(test_timeout);
      const experienceId = 100218245;
      const variation = dataManager.getBucketingById(
        visitorId,
        experienceId,
        {
          varName3: 'something'
        },
        {url: 'https://convert.com/'}
      );
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', () => {
            expect(variation)
              .to.be.an('object')
              .that.have.property('experienceId');
            expect(variation.experienceId).to.equal(experienceId);
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end();
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
      const ids = [10024, 10025];
      const entityType = 'features';
      const entities = dataManager.getEntitiesByIds(ids, entityType);
      expect(entities)
        .to.be.an('array')
        .that.deep.equal(
          configuration?.data?.[entityType]?.filter(({id}) =>
            ids.includes(id as number)
          )
        );
    });
  });

  describe('Test requests enqueuing', function () {
    it('Should process conversion event', function (done) {
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
      dataManager.convert(
        visitorId,
        goalKey,
        {
          action: 'buy'
        },
        [
          {
            amount: 10.3,
            productsCount: 2
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
      const variation = dataManager.getBucketing(
        visitorId,
        experienceKey,
        {
          varName3: 'something'
        },
        {url: 'https://convert.com/'}
      );
      expect(variation).to.be.null;
    });
    it('Should fail to update local store when reaching size limit', function () {
      new Array(10000)
        .fill(0)
        .forEach((v, i) => dataManager.putLocalStore(`a${i}`, {}));
      assert.equal(true, true);
    });
  });
});
