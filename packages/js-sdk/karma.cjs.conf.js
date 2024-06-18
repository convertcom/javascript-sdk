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

    files: [{pattern: './index.browser.cjs.tests.js'}],
    preprocessors: {
      './index.browser.cjs.tests.js': ['webpack']
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
