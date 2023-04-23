/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {Id, Feature, IdentityField, BucketedFeature} from '@convertcom/types';

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
    visitorProperties: Record<string, any>,
    locationProperties: Record<string, any>,
    typeCasting?: boolean,
    experienceKeys?: Array<string>,
    environment?: string
  ): BucketedFeature | Array<BucketedFeature>;
  isFeatureEnabled(
    visitorId: Id,
    featureKey: string,
    visitorProperties: Record<string, any>,
    locationProperties: Record<string, any>,
    experienceKeys?: Array<string>,
    environment?: string
  ): boolean;
  isFeatureDeclared(key: string): boolean;
  runFeatureById(
    visitorId: Id,
    featureId: Id,
    visitorProperties: Record<string, any>,
    locationProperties: Record<string, any>,
    typeCasting?: boolean,
    experienceIds?: Array<Id>,
    environment?: string
  ): BucketedFeature | Array<BucketedFeature>;
  runFeatures(
    visitorId: Id,
    visitorProperties: Record<string, any>,
    locationProperties: Record<string, any>,
    typeCasting?: boolean,
    filter?: Record<string, Array<string>>,
    environment?: string
  ): Array<BucketedFeature>;
}
