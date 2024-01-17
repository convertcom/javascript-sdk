/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

export type BucketingAttributes = {
  environment?: string;
  locationProperties?: Record<any, any>;
  visitorProperties?: Record<any, any>;
  typeCasting?: boolean;
  experienceKeys?: Array<string>;
  updateVisitorProperties?: boolean;
};
