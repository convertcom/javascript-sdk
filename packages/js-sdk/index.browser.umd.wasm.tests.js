// No SDK library imports here. A UMD script should be already loaded in browser by karma
import {assert} from 'chai';
import runTests from './index.wasm.tests';

describe('Karma browser tests for UMD bundle', function () {
  it('Should have an SDK instance in namespace', function () {
    // eslint-disable-next-line no-undef
    assert.isDefined(ConvertSDK);
  });
  // eslint-disable-next-line mocha/no-setup-in-describe,no-undef
  runTests(ConvertSDK);
});
