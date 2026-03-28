import * as ConvertSDK from './lib/index';
import runTests from './index.tests';
import runToolkitTests from './toolkit.browser.tests';
import './standalone.browser.tests';

describe('Karma browser tests for CommonJS bundle', function () {
  // eslint-disable-next-line mocha/no-setup-in-describe
  runTests(ConvertSDK);
  // eslint-disable-next-line mocha/no-setup-in-describe
  runToolkitTests();
});
