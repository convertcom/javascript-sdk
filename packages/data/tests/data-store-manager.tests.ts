/* eslint-disable mocha/consistent-spacing-between-blocks */
import 'mocha';
import {expect} from 'chai';
import {assert} from 'chai';

import {EventManager as em} from '@convertcom/js-sdk-event';
import {DataStoreManager as dsm} from '../src/data-store-manager';
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
const batch_size = 10;
const release_interval = 1000;
const configuration = objectDeepMerge(testConfig, defaultConfig, {
  dataStore,
  events: {
    batch_size,
    release_interval
  }
}) as unknown as ConfigType;
const eventManager = new em(configuration);

describe('DataStoreManager tests', function () {
  const storeKey = 'test-key',
    storeData = {
      bucketing: {
        exp1: 'var1',
        exp2: 'var2'
      },
      goals: {
        goal1: true,
        goal2: true
      },
      segments: {
        browser: 'CH',
        devices: 'ALLPH',
        source: 'test',
        campaign: 'test',
        visitorType: 'new',
        country: 'US',
        customSegments: ['seg1', 'seg2']
      }
    };
  let dataStoreManager;
  // eslint-disable-next-line mocha/no-hooks-for-single-case
  before(function () {
    dataStoreManager = new dsm(configuration, {
      dataStore,
      eventManager
    });
  });
  afterEach(function () {
    dataStore.data = {};
  });
  it('Should expose DataStoreManager', function () {
    assert.isDefined(dsm);
  });
  it('Imported entity should be a constructor of DataStoreManager instance', function () {
    expect(dsm)
      .to.be.a('function')
      .that.has.property('name')
      .which.equal('DataStoreManager');
  });
  describe('Test Data Store Manager', function () {
    it('Should successfully set visitor data immediately', function () {
      dataStoreManager.set(storeKey, storeData);
      const check = dataStoreManager.get(storeKey);
      expect(check).to.deep.equal(storeData);
    });
    it('Should successfully enqueue visitor data', function (done) {
      dataStoreManager.enqueue(storeKey, storeData);
      setTimeout(function () {
        const check = dataStoreManager.get(storeKey);
        expect(check).to.deep.equal(storeData);
        done();
      }, release_interval + 1);
    });
    it('Should have the correct shape for visitor data', function () {
      dataStoreManager.set(storeKey, storeData);
      const check = dataStoreManager.get(storeKey);
      expect(check).to.be.an('object');
      expect(check)
        .to.have.property('bucketing')
        .that.deep.equal(storeData.bucketing);
      expect(check).to.have.property('goals').that.deep.equal(storeData.goals);
      expect(check)
        .to.have.property('segments')
        .that.deep.equal(storeData.segments);
    });
  });
});
