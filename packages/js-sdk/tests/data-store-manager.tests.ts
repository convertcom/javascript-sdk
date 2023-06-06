import 'mocha';
import {expect} from 'chai';
import {assert} from 'chai';

import {DataStoreManager as dsm} from '../src/data-store-manager';
import configuration from './test-config.json';

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
