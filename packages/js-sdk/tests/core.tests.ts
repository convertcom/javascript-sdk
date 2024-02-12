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
import {Core as c} from '../src/core';
import {Context} from '../src/context';
import testConfig from './test-config.json';
import {Config} from '../src/config';
import {Config as ConfigType} from '@convertcom/js-sdk-types';
import {objectDeepMerge} from '@convertcom/js-sdk-utils';
import {defaultConfig} from '../src/config/default';
import {SystemEvents, ERROR_MESSAGES} from '@convertcom/js-sdk-enums';

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

describe('Core tests', function () {
  it('Imported entity should be a constructor of Core instance', function () {
    expect(c).to.be.a('function').that.has.property('name').which.equal('Core');
  });
  it('Should successfully create new Core instance', async function () {
    const dataManager = new dm(configuration, {
      bucketingManager,
      ruleManager,
      eventManager,
      apiManager
    });
    const experienceManager = new exm(configuration, {dataManager});
    const featureManager = new fm(configuration, {dataManager});
    const segmentsManager = new sm(configuration, {dataManager, ruleManager});
    const core = new c(configuration, {
      eventManager,
      experienceManager,
      featureManager,
      segmentsManager,
      dataManager,
      apiManager
    });
    expect(core)
      .to.be.an('object')
      .that.has.property('constructor')
      .that.has.property('name')
      .which.equal('Core');
  });

  describe('Test Core', function () {
    let dataManager,
      experienceManager,
      featureManager,
      segmentsManager,
      core,
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
      core = new c(configuration, {
        eventManager,
        experienceManager,
        featureManager,
        segmentsManager,
        dataManager,
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
      server.close();
    });
    it('Should expose Core', function () {
      assert.isDefined(c);
    });
    it('Shoud successfully create visitor context', function () {
      const visitorId = 'XXX';
      const visitorContext = core.createContext(visitorId, {browser: 'chrome'});
      expect(visitorContext).to.be.an.instanceof(Context);
    });
    it('Shoud successfully trigger ready event', function (done) {
      core.on(SystemEvents.READY, function (args, err) {
        expect(err).to.be.null;
        eventManager.removeListeners(SystemEvents.READY);
        done();
      });
    });
    it('Shoud successfully resolve onReady promise', async function () {
      await core.onReady();
      assert.equal(true, true);
    });
    it('Shoud successfully get config using sdk key', function (done) {
      this.timeout(test_timeout);

      server.on('request', (request, res) => {
        if (request.url.startsWith(`/config/${accountId}/${projectId}`)) {
          request.on('end', () => {
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end();
      });

      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const {data: _data, ...config} = configuration;
      core = new c(
        {...config, sdkKey: `${accountId}/${projectId}`},
        {
          eventManager,
          experienceManager,
          featureManager,
          segmentsManager,
          dataManager,
          apiManager
        }
      );
    });
  });
  describe('Test invalid config', function () {
    let dataManager,
      experienceManager,
      featureManager,
      segmentsManager,
      core,
      accountId,
      projectId,
      server;
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    before(function () {
      accountId = configuration?.data?.account_id;
      projectId = configuration?.data?.project?.id;
      dataManager = new dm(null, {
        bucketingManager,
        ruleManager,
        eventManager,
        apiManager
      });
      experienceManager = new exm(null, {dataManager});
      featureManager = new fm(null, {dataManager});
      segmentsManager = new sm(null, {dataManager, ruleManager});
    });
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    beforeEach(function () {
      server = http.createServer();
      server.listen(port);
    });
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    afterEach(function () {
      server.close();
    });
    it('Shoud fail to inisialize if config data is invalid', function (done) {
      core = new c(
        // eslint-disable-next-line
        // @ts-ignore
        {},
        {
          eventManager,
          experienceManager,
          featureManager,
          segmentsManager,
          dataManager,
          apiManager
        }
      );
      core.on(SystemEvents.READY, function (args, err) {
        expect(err).to.be.an.instanceOf(Error);
        expect(err.message).to.equal(
          ERROR_MESSAGES.SDK_OR_DATA_OBJECT_REQUIRED
        );
        eventManager.removeListeners(SystemEvents.READY);
        done();
      });
    });
    it('Shoud fail to inisiapConflize if config data is not set', function () {
      core = new c(null, {
        eventManager,
        experienceManager,
        featureManager,
        segmentsManager,
        dataManager,
        apiManager
      });
      expect(dataManager.data).to.be.null;
    });
    it('Shoud fail to inisialize if config data returns server-side errors', async function () {
      const error = 'this account was canceled';
      core = new c({...configuration, data: {error}} as ConfigType, {
        eventManager,
        experienceManager,
        featureManager,
        segmentsManager,
        dataManager,
        apiManager
      });
      expect(dataManager.data).to.be.an('object');
      expect(dataManager.data).to.have.property('error');
      expect(dataManager.data.error).to.equal(error);
    });
    it('Shoud fail to resolve onReady promise if config data is not set', async function () {
      this.timeout(test_timeout);

      server.on('request', (request, res) => {
        if (request.url.startsWith(`/config/${accountId}/${projectId}`)) {
          res.writeHead(200, {'Content-Type': 'application/json'});
          res.end();
        }
      });

      const config = Config({
        api: {
          endpoint: {
            config: host + ':' + port,
            track: host + ':' + port
          }
        },
        events: {
          batch_size: batch_size,
          release_interval: release_timeout
        },
        sdkKey: `${accountId}/${projectId}`
      });
      core = new c(config, {
        eventManager,
        experienceManager,
        featureManager,
        segmentsManager,
        dataManager,
        apiManager
      });
      try {
        await core.onReady();
      } catch (error) {
        expect(error.message).to.equal(ERROR_MESSAGES.DATA_OBJECT_MISSING);
      }
    });
  });
});
