import 'mocha';
import {expect} from 'chai';
import {assert} from 'chai';

import {DataStoreManager as dsm} from '@convertcom/js-sdk-data';
import testConfig from './test-config.json';
import {Config as ConfigType} from '@convertcom/js-sdk-types';
import {objectDeepMerge} from '@convertcom/js-sdk-utils';
import {defaultConfig} from '../src/config/default';

const configuration = objectDeepMerge(
  testConfig,
  defaultConfig
) as unknown as ConfigType;

describe('DataStoreManager tests', function () {
  it('Should expose DataStoreManager', function () {
    assert.isDefined(dsm);
  });
  it('Imported entity should be a constructor of DataStoreManager instance', function () {
    expect(dsm)
      .to.be.a('function')
      .that.has.property('name')
      .which.equal('DataStoreManager');
  });
});
