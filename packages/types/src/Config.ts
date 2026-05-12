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
   * to RuleManager when no per-call visitor/location/goal-rule
   * arguments are supplied. The provider's `name` MUST be the literal
   * string `'RuleData'`; RuleManager uses that discriminator to switch
   * from key lookup to method dispatch.
   *
   * **Precedence: per-call arguments win.** Passing
   * `visitorProperties`, `locationProperties`, or `trackConversion`'s
   * `ruleData` on a specific call bypasses the provider for that call.
   * See `@convertcom/js-sdk-types#RuleDataProvider` for the full
   * contract.
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
