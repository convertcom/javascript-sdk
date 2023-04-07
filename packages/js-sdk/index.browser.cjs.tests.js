import * as ConvertSDK from './lib/index';
import runTests from './index.tests';

describe('Karma browser tests for CommonJS bundle', function () {
  // eslint-disable-next-line mocha/no-setup-in-describe
  runTests(ConvertSDK);
});
