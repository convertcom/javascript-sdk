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
  Entity,
  SegmentsAttributes,
  StoreData,
  VisitorSegments
} from '@convertcom/js-sdk-types';
import {BucketingError, EntityType, RuleError} from '@convertcom/js-sdk-enums';

/**
 * qs-02: input to {@link ContextInterface.setPreview}.
 */
export interface PreviewInput {
  experienceId: string;
  variationId: string;
}

export interface ContextInterface {
  setPreview(input: PreviewInput): Promise<void>;

  runExperience(
    experienceKey: string,
    attributes?: BucketingAttributes
  ): BucketedVariation | RuleError | BucketingError;

  runExperiences(
    attributes?: BucketingAttributes
  ): Array<BucketedVariation | RuleError | BucketingError>;

  runFeature(
    key: string,
    attributes?: BucketingAttributes
  ): BucketedFeature | RuleError | Array<BucketedFeature | RuleError>;

  runFeatures(
    attributes?: BucketingAttributes
  ): Array<BucketedFeature | RuleError>;

  trackConversion(
    goalKey: string,
    attributes?: ConversionAttributes
  ): RuleError;

  setDefaultSegments(segments: VisitorSegments): void;

  runCustomSegments(
    segmentKeys: Array<string>,
    attributes?: SegmentsAttributes
  ): RuleError;

  updateVisitorProperties(
    visitorId: string,
    visitorProperties: Record<string, any>
  ): void;

  getConfigEntity(key: string, entityType: EntityType): Entity;
  getConfigEntityById(id: string, entityType: EntityType): Entity;
  getVisitorData(): StoreData;

  releaseQueues(reason?: string): Promise<any>;
}
