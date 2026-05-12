/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {ConfigResponseData} from './config/index';
import {RuleDataProvider} from './RuleDataProvider';
import {LogLevel} from '@convertcom/js-sdk-enums';

export * from './config/index';

type ConfigBase = {
  environment: string;
  api?: {
    endpoint?: {
      config?: string;
      track?: string;
    };
  };
  bucketing?: {
    hash_seed?: number;
    max_traffic?: number;
    excludeExperienceIdHash?: boolean;
  };
  dataStore?: object | null;
  /**
   * Optional custom RuleData provider for evaluating web rule types
   * (URL, cookie, geo, device, etc.). When set, DataManager passes it
   * to RuleManager instead of the plain visitor/location properties.
   * The provider's `name` MUST be the literal string `'RuleData'`;
   * RuleManager uses that discriminator to switch from key lookup to
   * method dispatch.
   */
  ruleDataProvider?: RuleDataProvider;
  dataRefreshInterval?: number;
  events?: {
    batch_size?: number;
    release_interval?: number;
  };
  rules?: {
    comparisonProcessor?: Record<string, any>;
    negation?: string;
    keys_case_sensitive?: boolean;
  };
  logger?: {
    logLevel?: LogLevel;
    file?: {
      enabled?: boolean;
      path?: string;
      logLevel?: LogLevel;
    };
    customLoggers?: Array<Record<string, any>>;
  };
  network?: {
    tracking?: boolean;
    cacheLevel?: string;
    source?: string;
  };
  mapper?: (...args: any) => any;
};

type ConfigWithSdkKey = ConfigBase & {
  sdkKey: string;
  sdkKeySecret?: string;
  data?: never;
};

type ConfigWithData = ConfigBase & {
  data: ConfigResponseData;
  sdkKey?: never;
  sdkKeySecret?: never;
};

export type Config = ConfigWithSdkKey | ConfigWithData;
