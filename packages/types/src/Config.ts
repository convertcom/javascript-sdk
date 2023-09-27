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

export type ConfigData = {
  account_id: Id;
  project: Project;
  goals?: Array<Goal>;
  locations?: Array<Location>;
  audiences?: Array<Audience>;
  experiences?: Array<Experience>;
  archived_experiences?: Array<Id>;
  features?: Array<Feature>;
  segments?: Array<Segments>;
};

export type Config = {
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
  data: ConfigData;
  dataStore?: object;
  dataRefreshInterval?: number;
  tracking?: boolean;
  events?: {
    batch_size?: number;
    release_interval?: number;
  };
  rules?: {
    comparisonProcessor?: Record<string, any>;
    negation?: string;
    keys_case_sensitive?: boolean;
  };
  sdkKey: string;
  logger?: {
    logLevel?: LogLevel;
    file?: {
      enabled?: boolean;
      path?: string;
      logLevel?: LogLevel;
    };
    customLoggers: Array<Record<string, any>>;
  };
};
