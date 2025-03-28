/*!
 * Convert JS SDK config
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {LogLevel} from '@convertcom/js-sdk-enums';

export const defaultConfig = {
  api: {
    endpoint: {
      config: process.env.CONFIG_ENDPOINT,
      track: process.env.TRACK_ENDPOINT
    }
  },
  environment: 'staging',
  bucketing: {
    max_traffic: 10000,
    hash_seed: 9999
  },
  data: {},
  dataStore: null, // Allows 3rd party data store to be passed
  dataRefreshInterval: 300000, // in milliseconds (5 minutes)
  events: {
    batch_size: 10,
    release_interval: 1000
  },
  logger: {
    logLevel: LogLevel.DEBUG,
    customLoggers: [] // Allows 3rd party loggers to be passed
  },
  rules: {
    keys_case_sensitive: true,
    comparisonProcessor: null // Allows 3rd party comparison processor to be passed
  },
  network: {
    tracking: true,
    cacheLevel: 'default' // can be set to 'low' for short-lived cache (for development purposes only)
  },
  sdkKey: '',
  sdkKeySecret: ''
};
