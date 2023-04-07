/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {Config as ConfigType} from './types/Config';
import {defaultConfig} from './config/default';
import {LogLevel} from './enums/log-level';
import {objectDeepMerge} from './utils/object-utils';

const DEFAULT_LOGGER_SETTINGS = {
  logger: {
    logLevel: LogLevel.WARN,
    customLoggers: []
  }
};
const DEFAULT_ENVIRONMENT_SETTINGS = {
  environment: 'staging'
};

/**
 * @param {Record<any, any>=} config
 * @return {ConfigType}
 */
export const Config = (config: Record<any, any> = {}): ConfigType => {
  const configuration = objectDeepMerge(
    DEFAULT_LOGGER_SETTINGS,
    DEFAULT_ENVIRONMENT_SETTINGS,
    defaultConfig,
    config
  );
  return configuration as ConfigType;
};
