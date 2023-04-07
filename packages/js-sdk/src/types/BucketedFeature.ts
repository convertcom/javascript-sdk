/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {Id} from './Id';
import {FeatureStatus} from '../enums/feature-status';
import {RequireAtLeastOne} from './RequireAtLeastOne';

// Not enabled feature can have only disabled status but have to contain id or key
type NotEnabledFeature = RequireAtLeastOne<
  {
    experienceId?: Id;
    experienceKey?: string;
    experienceName?: string;
    id?: Id;
    key?: string;
    name?: string;
    status: FeatureStatus.DISABLED;
    variables?: Record<string, any>;
  },
  'id' | 'key'
>;

export type BucketedFeature =
  | {
      experienceId?: Id;
      experienceKey?: string;
      experienceName?: string;
      id?: Id;
      key?: string;
      name?: string;
      status: FeatureStatus;
      variables?: Record<string, any>;
    }
  | NotEnabledFeature;
