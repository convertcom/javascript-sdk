import 'mocha';
import {expect} from 'chai';
import {assert} from 'chai';

import {DataManager as dm} from '../src/data-manager';
import testConfig from './test-config.json';
import {Config} from '../src/config';
const configuration = Config(testConfig);

describe('DataManager tests', function () {
  it('Should expose DataManager', function () {
    assert.isDefined(dm);
  });
  it('Imported entity should be a constructor of DataManager instance', function () {
    expect(dm)
      .to.be.a('function')
      .that.has.property('name')
      .which.equal('DataManager');
  });
});
