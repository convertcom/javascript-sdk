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

describe('ExperienceManager tests', function () {
  const visitorId = 'XXX';
  let dataManager, experienceManager, accountId, projectId, server;
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
  it('Should expose ExperienceManager', function () {
    assert.isDefined(exm);
  });
  it('Imported entity should be a constructor of ExperienceManager instance', function () {
    expect(exm)
      .to.be.a('function')
      .that.has.property('name')
      .which.equal('ExperienceManager');
  });
  it('Should successfully create new ExperienceManager instance', async function () {
    expect(experienceManager)
      .to.be.an('object')
      .that.has.property('constructor')
      .that.has.property('name')
      .which.equal('ExperienceManager');
  });

  describe('Test Experience Manager', function () {
    it('Shoud successfully get a list of all entities', function () {
      const entities = experienceManager.getList();
      expect(entities)
        .to.be.an('array')
        .that.has.length(3)
        .to.deep.equal(configuration?.data?.experiences);
    });
    it('Shoud successfully get the entity by key', function () {
      const experienceKey = 'test-experience-ab-fullstack-2';
      const experienceId = 100218245;
      const entity = experienceManager.getExperience(experienceKey);
      expect(entity)
        .to.be.an('object')
        .that.has.property('id')
        .to.equal(experienceId);
    });
    it('Shoud successfully get the entity by id', function () {
      const experienceKey = 'test-experience-ab-fullstack-2';
      const experienceId = 100218245;
      const entity = experienceManager.getExperienceById(experienceId);
      expect(entity)
        .to.be.an('object')
        .that.has.property('key')
        .to.equal(experienceKey);
    });
    it('Shoud successfully specific entities by array of keys', function () {
      const experienceKeys = [
        'test-experience-ab-fullstack-2',
        'test-experience-ab-fullstack-3',
        'test-experience-ab-fullstack-4'
      ];
      const entities = experienceManager.getExperiences(experienceKeys);
      expect(entities)
        .to.be.an('array')
        .to.deep.equal(configuration?.data?.experiences);
    });
    it('Shoud successfully select variation for specific visitor', function (done) {
      this.timeout(test_timeout);
      const experienceKey = 'test-experience-ab-fullstack-2';
      const variation = experienceManager.selectVariation(
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
    it('Shoud successfully select variation for specific visitor by id', function (done) {
      this.timeout(test_timeout);
      const experienceId = 100218245;
      const variation = experienceManager.selectVariationById(
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
    it('Shoud successfully select all variations across all experiences for specific visitor', function (done) {
      this.timeout(test_timeout);
      const variationIds = [100299456, 100299457, 100299460, 100299461];
      const variations = experienceManager.selectVariations(
        visitorId,
        {
          varName3: 'something'
        },
        {url: 'https://convert.com/'}
      );
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', () => {
            expect(variations).to.be.an('array').that.have.length(2);
            const selectedVariations = variations.map(({id}) => id);
            expect(variationIds).to.include.deep.members(selectedVariations);
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end();
      });
    });
    it('Shoud successfully get experience variation by key', function () {
      const experienceKey = 'test-experience-ab-fullstack-2';
      const variationKey = '100299457-variation-1';
      const variationId = 100299457;
      const variation = experienceManager.getVariation(
        experienceKey,
        variationKey
      );
      expect(variation)
        .to.be.an('object')
        .that.has.property('id')
        .to.equal(variationId);
    });
    it('Shoud successfully get experience variation by id', function () {
      const experienceId = 100218245;
      const variationKey = '100299457-variation-1';
      const variationId = 100299457;
      const variation = experienceManager.getVariationById(
        experienceId,
        variationId
      );
      expect(variation)
        .to.be.an('object')
        .that.has.property('key')
        .to.equal(variationKey);
    });
  });
});
