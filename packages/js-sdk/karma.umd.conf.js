// Karma configuration
// eslint-disable-next-line @typescript-eslint/no-var-requires
const baseConfig = require('./karma.base.conf.js');
// eslint-disable-next-line @typescript-eslint/no-var-requires
const path = require('path');
// eslint-disable-next-line @typescript-eslint/no-var-requires
const os = require('os');
module.exports = function (config) {
  config.set({
    ...baseConfig,
    // list of files / patterns to load in the browser
    files: [
      './lib/index.umd.min.js',
      './index.browser.umd.tests.js',
      // Serve WASM file but don't include it in the test runner
      {pattern: './lib/decisions_core_bg.wasm', included: false, served: true, watched: false}
    ],
    // preprocess matching files before serving them to the browser
    // available preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
    preprocessors: {
      './index.browser.umd.tests.js': ['webpack']
    },
    webpack: {
      output: {
        filename: '[name].js',
        path:
          path.join(os.tmpdir(), '_karma_webpack_') +
          Math.floor(Math.random() * 1000000)
      }
    },
    // level of logging
    // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    logLevel: config.LOG_INFO
  });
};
