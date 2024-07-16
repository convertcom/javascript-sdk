/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {ExperienceVariationConfig} from './config/index';

export type BucketedVariation = ExperienceVariationConfig & {
  experienceId?: string;
  experienceKey?: string;
  experienceName?: string;
  bucketingAllocation?: number;
};
