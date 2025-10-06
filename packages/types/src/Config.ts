/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {ConfigResponseData} from './config/index';
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
  experimental?: {
    useRustDecider?: boolean;
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
