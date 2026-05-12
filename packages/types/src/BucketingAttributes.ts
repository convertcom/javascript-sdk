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
  /**
   * Optional filter restricting which experience types are considered
   * eligible for bucketing.
   *
   *   undefined  → no filter, all types are considered (default)
   *   []         → zero types allowed → no experiences match
   *                (matches standard array-filter intuition; useful for
   *                guarding a code path that should never bucket)
   *   [t1, t2..] → only experiences whose `type` is in the list
   *
   * Applies to `runExperience`, `runExperiences`, `runFeature`, and
   * `runFeatures`. Pass `undefined` (or omit) to get the previous
   * "all types" behavior.
   */
  experienceTypes?: Array<ExperienceTypes>;
  updateVisitorProperties?: boolean;
  forceVariationId?: string;
  enableTracking?: boolean;
  ignoreLocationProperties?: boolean;
};
