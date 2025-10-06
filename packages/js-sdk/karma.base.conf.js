// eslint-disable-next-line @typescript-eslint/no-var-requires
const dotenv = require('dotenv');
// eslint-disable-next-line @typescript-eslint/no-var-requires
const path = require('path');
dotenv.config();
// eslint-disable-next-line @typescript-eslint/no-var-requires
process.env.CHROME_BIN = require('puppeteer').executablePath();
module.exports = {
  // base path that will be used to resolve all patterns (eg. files, exclude)
  basePath: './',

  // frameworks to use
  // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
  frameworks: ['mocha', 'chai', 'webpack'],

  // list of files / patterns to load in the browser
  files: [],

  // list of files / patterns to exclude
  exclude: [],

  // preprocess matching files before serving them to the browser
  // available preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
  preprocessors: {},

  plugins: [
    'karma-mocha',
    'karma-chai',
    'karma-webpack',
    'karma-chrome-launcher',
    require('karma-browserstack-launcher'),
    'karma-mocha-reporter'
  ],
  // test results reporter to use
  // possible values: 'dots', 'progress'
  // available reporters: https://npmjs.org/browse/keyword/karma-reporter
  // reporters: ['progress'],
  reporters: ['mocha', 'BrowserStack'],

  // web server port
  port: 9876,

  // enable / disable colors in the output (reporters and logs)
  colors: true,

  // enable / disable watching file and executing tests whenever any file changes
  autoWatch: false,

  // Continuous Integration mode
  // if true, Karma captures browsers, runs the tests and exits
  singleRun: true,

  // Concurrency level
  // how many browser should be started simultaneous
  concurrency: Infinity,

  browserStack: {
    username: process.env.BROWSER_STACK_USERNAME,
    accessKey: process.env.BROWSER_STACK_ACCESS,
    startTunnel: true
  },
  webpack: {
    mode: 'production',
    devtool: 'inline-source-map',
    performance: {
      hints: false
    },
    experiments: {
      asyncWebAssembly: true
    },
    module: {
      rules: [
        {
          test: /decisions_core_bg\.wasm$/,
          type: 'asset/resource',
          generator: {
            filename: '[name][ext]'
          }
        }
      ]
    }
  },
  // define browsers
  customLaunchers: {
    bs_firefox100_win10: {
      base: 'BrowserStack',
      os: 'Windows',
      os_version: '10',
      browser: 'firefox',
      device: null,
      browser_version: '100.0',
      real_mobile: null
    },
    bs_chrome_100_osx: {
      base: 'BrowserStack',
      os: 'OS X',
      os_version: 'Mojave',
      browser: 'chrome',
      device: null,
      browser_version: '100.0',
      real_mobile: null
    },
    custom_chrome_headless: {
      base: 'ChromeHeadless',
      chromeDataDir: path.resolve(__dirname, 'webpack-build/.chrome-karma'),
      flags: [
        '--disable-gpu',
        '--no-sandbox',
        '--crash-dumps-dir=' +
          path.resolve(__dirname, 'webpack-build/.chrome-karma-crash')
      ]
    }
  },

  // start these browsers
  // available browser launchers: https://npmjs.org/browse/keyword/karma-launcher
  browsers: ['custom_chrome_headless']
};
