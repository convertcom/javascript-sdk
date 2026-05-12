/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {ExperienceTypes} from './config/index';

export type BucketingAttributes = {
  environment?: string;
  locationProperties?: Record<any, any>;
  visitorProperties?: Record<any, any>;
  typeCasting?: boolean;
  experienceKeys?: Array<string>;
  experienceTypes?: Array<ExperienceTypes>;
  updateVisitorProperties?: boolean;
  forceVariationId?: string;
  enableTracking?: boolean;
  ignoreLocationProperties?: boolean;
};
