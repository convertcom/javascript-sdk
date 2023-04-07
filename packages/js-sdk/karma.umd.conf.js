// Karma configuration
// eslint-disable-next-line @typescript-eslint/no-var-requires
const baseConfig = require('./karma.base.conf.js');
module.exports = function (config) {
  config.set({
    ...baseConfig,
    // list of files / patterns to load in the browser
    files: ['./lib/index.umd.min.js', './index.browser.umd.tests.js'],
    // preprocess matching files before serving them to the browser
    // available preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
    preprocessors: {
      './index.browser.umd.tests.js': ['webpack']
    },
    // level of logging
    // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    logLevel: config.LOG_INFO
  });
};
