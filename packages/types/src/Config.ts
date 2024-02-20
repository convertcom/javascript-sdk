/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {Experience} from './Experience';
import {Feature} from './Feature';
import {Id} from './Id';
import {Project} from './Project';
import {Goal} from './Goal';
import {Audience} from './Audience';
import {Location} from './Location';
import {LogLevel} from '@convertcom/js-sdk-enums';
import {Segments} from './Segments';
import {RequireAtLeastOne} from './RequireAtLeastOne';

export type ConfigData = RequireAtLeastOne<{
  error: string;
  account_id: Id;
  project: Project;
  goals: Array<Goal>;
  locations: Array<Location>;
  audiences: Array<Audience>;
  experiences: Array<Experience>;
  archived_experiences: Array<Id>;
  features: Array<Feature>;
  segments: Array<Segments>;
}>;

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
  };
  dataStore?: object;
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
    customLoggers: Array<Record<string, any>>;
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
  data?: never;
};

type ConfigWithData = ConfigBase & {
  data: ConfigData;
  sdkKey?: never;
};

export type Config = ConfigWithSdkKey | ConfigWithData;
