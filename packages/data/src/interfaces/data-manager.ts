/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {
  Entity,
  ConfigResponseData,
  IdentityField,
  BucketedVariation,
  StoreData,
  VisitorSegments,
  ConfigExperience,
  BucketingAttributes,
  LocationAttributes
} from '@convertcom/js-sdk-types';
import {DataStoreManagerInterface} from './data-store-manager';
import {
  BucketingError,
  ConversionSettingKey,
  GoalDataKey,
  RuleError
} from '@convertcom/js-sdk-enums';

export interface DataManagerInterface {
  data: ConfigResponseData;
  dataStoreManager: DataStoreManagerInterface;

  reset();
  putData(storeKey: string, storeData: StoreData);
  getData(storeKey: string): StoreData;
  getStoreKey(visitorId: string): string;
  selectLocations(
    visitorId: string,
    items: Array<Record<string, any>>,
    attributes: LocationAttributes
  ): Array<Record<string, any> | RuleError>;
  matchRulesByField(
    visitorId: string,
    identity: string,
    identityField: IdentityField,
    attributes: BucketingAttributes
  ): ConfigExperience | RuleError;
  getBucketing(
    visitorId: string,
    experienceKey: string,
    attributes: BucketingAttributes
  ): BucketedVariation | RuleError | BucketingError;
  getBucketingById(
    visitorId: string,
    experienceId: string,
    attributes: BucketingAttributes
  ): BucketedVariation | RuleError | BucketingError;
  convert(
    visitorId: string,
    goalId: string,
    goalRule?: Record<string, any>,
    goalData?: Array<Record<GoalDataKey, number>>,
    segments?: VisitorSegments,
    conversionSetting?: Record<ConversionSettingKey, number | string | boolean>
  ): RuleError | boolean;
  getEntitiesList(entityType: string): Array<Entity | string>;
  getEntitiesListObject(
    entityType: string,
    field?: IdentityField
  ): Record<string, Entity>;
  getEntity(key: string, entityType: string): Entity;
  getEntities(keys: Array<string>, entityType: string): Array<Entity>;
  getEntityById(id: string, entityType: string): Entity;
  getEntitiesByIds(ids: Array<string>, entityType: string): Array<Entity>;
  getItemsByKeys(keys: Array<string>, path: string): Array<Record<string, any>>;
  getItemsByIds(ids: Array<string>, path: string): Array<Record<string, any>>;
  getSubItem(
    entityType: string,
    entityIdentity: string,
    subEntityType: string,
    subEntityIdentity: string,
    identityField: IdentityField,
    subIdentityField: IdentityField
  ): Record<any, any>;

  filterReportSegments(
    visitorProperties: Record<string, any>
  ): Record<string, any>;

  isValidConfigData(data: ConfigResponseData): boolean;
  setDataStore(dataStore: any): void;
}
