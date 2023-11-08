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
  BucketedFeature
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
    visitorProperties: Record<string, any> | null,
    locationProperties: Record<string, any> | null,
    typeCasting?: boolean,
    experienceKeys?: Array<string>,
    environment?: string
  ): BucketedFeature | RuleError | Array<BucketedFeature | RuleError>;
  isFeatureEnabled(
    visitorId: Id,
    featureKey: string,
    visitorProperties: Record<string, any> | null,
    locationProperties: Record<string, any> | null,
    experienceKeys?: Array<string>,
    environment?: string
  ): boolean;
  isFeatureDeclared(key: string): boolean;
  runFeatureById(
    visitorId: Id,
    featureId: Id,
    visitorProperties: Record<string, any> | null,
    locationProperties: Record<string, any> | null,
    typeCasting?: boolean,
    experienceIds?: Array<Id>,
    environment?: string
  ): BucketedFeature | RuleError | Array<BucketedFeature | RuleError>;
  runFeatures(
    visitorId: Id,
    visitorProperties: Record<string, any>,
    locationProperties: Record<string, any>,
    typeCasting?: boolean,
    filter?: Record<string, Array<string>>,
    environment?: string
  ): Array<BucketedFeature | RuleError>;
}
