/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {BucketedFeature} from '../types/BucketedFeature';
import {BucketedVariation} from '../types/BucketedVariation';
import {BucketingAttributes} from '../types/BucketingAttributes';
import {ConversionAttributes} from '../types/ConversionAttributes';
import {Id} from '../types/Id';
import {SegmentsAttributes} from '../types/SegmentsAttributes';
import {SegmentsData} from '../types/SegmentsData';

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
