// Karma configuration
// eslint-disable-next-line @typescript-eslint/no-var-requires
const baseConfig = require('./karma.base.conf.js');
module.exports = function (config) {
  config.set({
    ...baseConfig,

    files: [{pattern: './index.browser.cjs.tests.js'}],
    preprocessors: {
      './index.browser.cjs.tests.js': ['webpack']
    },
    // level of logging
    // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    logLevel: config.LOG_INFO
  });
};
