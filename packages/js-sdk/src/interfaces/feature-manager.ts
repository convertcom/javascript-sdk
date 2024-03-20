/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {
  ConfigFeature,
  IdentityField,
  BucketedFeature,
  BucketingAttributes
} from '@convertcom/js-sdk-types';
import {RuleError} from '@convertcom/js-sdk-enums';

export interface FeatureManagerInterface {
  getList(): Array<ConfigFeature>;
  getFeature(key: string): ConfigFeature;
  getFeatureById(id: string): ConfigFeature;
  getFeatures(keys: Array<string>): Array<ConfigFeature>;
  getListAsObject(field: IdentityField): Record<string, ConfigFeature>;
  getFeatureVariableType(key: string, variableName: string): string;
  getFeatureVariableTypeById(id: string, variableName: string): string;

  runFeature(
    visitorId: string,
    featureKey: string,
    attributes: BucketingAttributes,
    experienceKeys?: Array<string>
  ): BucketedFeature | RuleError | Array<BucketedFeature | RuleError>;
  isFeatureEnabled(
    visitorId: string,
    featureKey: string,
    attributes: BucketingAttributes,
    experienceKeys?: Array<string>
  ): boolean;
  isFeatureDeclared(key: string): boolean;
  runFeatureById(
    visitorId: string,
    featureId: string,
    attributes: BucketingAttributes,
    experienceIds?: Array<string>
  ): BucketedFeature | RuleError | Array<BucketedFeature | RuleError>;
  runFeatures(
    visitorId: string,
    attributes: BucketingAttributes,
    filter?: Record<string, Array<string>>
  ): Array<BucketedFeature | RuleError>;
}
