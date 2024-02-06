/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {
  Id,
  Feature,
  IdentityField,
  BucketedFeature,
  BucketingAttributes
} from '@convertcom/js-sdk-types';
import {RuleError} from '@convertcom/js-sdk-enums';

export interface FeatureManagerInterface {
  getList(): Array<Feature>;
  getFeature(key: string): Feature;
  getFeatureById(id: Id): Feature;
  getFeatures(keys: Array<string>): Array<Feature>;
  getListAsObject(field: IdentityField): Record<string, Feature>;
  getFeatureVariableType(key: string, variableName: string): string;
  getFeatureVariableTypeById(id: Id, variableName: string): string;

  runFeature(
    visitorId: Id,
    featureKey: string,
    attributes: BucketingAttributes,
    experienceKeys?: Array<string>
  ): BucketedFeature | RuleError | Array<BucketedFeature | RuleError>;
  isFeatureEnabled(
    visitorId: Id,
    featureKey: string,
    attributes: BucketingAttributes,
    experienceKeys?: Array<string>
  ): boolean;
  isFeatureDeclared(key: string): boolean;
  runFeatureById(
    visitorId: Id,
    featureId: Id,
    attributes: BucketingAttributes,
    experienceIds?: Array<Id>
  ): BucketedFeature | RuleError | Array<BucketedFeature | RuleError>;
  runFeatures(
    visitorId: Id,
    attributes: BucketingAttributes,
    filter?: Record<string, Array<string>>
  ): Array<BucketedFeature | RuleError>;
}
