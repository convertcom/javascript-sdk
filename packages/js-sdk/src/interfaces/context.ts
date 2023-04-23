/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {
  BucketedFeature,
  BucketedVariation,
  BucketingAttributes,
  ConversionAttributes,
  Id,
  SegmentsAttributes,
  SegmentsData
} from '@convertcom/types';

export interface ContextInterface {
  runExperience(
    experienceKey: string,
    attributes?: BucketingAttributes
  ): BucketedVariation | null;

  runExperiences(attributes?: BucketingAttributes): Array<BucketedVariation>;

  runFeature(
    key: string,
    attributes?: BucketingAttributes
  ): BucketedFeature | Array<BucketedFeature>;

  runFeatures(attributes?: BucketingAttributes): Array<BucketedFeature>;

  trackConversion(goalKey: Id, attributes?: ConversionAttributes): void;

  setDefaultSegments(segments: SegmentsData): void;

  setCustomSegments(
    segmentKeys: Array<string>,
    attributes?: SegmentsAttributes
  ): void;
}
