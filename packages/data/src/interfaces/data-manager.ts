/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {
  Entity,
  ConfigData,
  Id,
  IdentityField,
  BucketedVariation,
  StoreData,
  SegmentsData,
  Experience,
  BucketingAttributes
} from '@convertcom/js-sdk-types';
import {DataStoreManagerInterface} from './data-store-manager';
import {GoalDataKey, RuleError} from '@convertcom/js-sdk-enums';

export interface DataManagerInterface {
  data: ConfigData;
  dataStoreManager: DataStoreManagerInterface;

  reset();
  putData(storeKey: Id, storeData: StoreData);
  getData(storeKey: Id): StoreData;
  getStoreKey(visitorId: Id): string;
  selectLocations(
    visitorId: string,
    items: Array<Record<string, any>>,
    locationProperties: Record<string, any>,
    identityField: IdentityField
  ): Array<Record<string, any> | RuleError>;
  matchRulesByField(
    visitorId: string,
    identity: string | Id,
    identityField: IdentityField,
    attributes: BucketingAttributes
  ): Experience | RuleError;
  getBucketing(
    visitorId: Id,
    experienceKey: string,
    attributes: BucketingAttributes
  ): BucketedVariation | RuleError;
  getBucketingById(
    visitorId: Id,
    experienceId: Id,
    attributes: BucketingAttributes
  ): BucketedVariation | RuleError;
  convert(
    visitorId: Id,
    goalId: Id,
    goalRule?: Record<string, any>,
    goalData?: Array<Record<GoalDataKey, number>>,
    segments?: SegmentsData
  ): RuleError | boolean;
  getEntitiesList(entityType: string): Array<Entity | Id>;
  getEntitiesListObject(
    entityType: string,
    field?: IdentityField
  ): Record<string, Entity>;
  getEntity(key: string, entityType: string): Entity;
  getEntities(keys: Array<string>, entityType: string): Array<Entity>;
  getEntityById(id: Id, entityType: string): Entity;
  getEntitiesByIds(ids: Array<Id>, entityType: string): Array<Entity>;
  getItemsByKeys(keys: Array<string>, path: string): Array<Record<string, any>>;
  getItemsByIds(ids: Array<Id>, path: string): Array<Record<string, any>>;
  getSubItem(
    entityType: string,
    entityIdentity: string | number,
    subEntityType: string,
    subEntityIdentity: string | number,
    identityField: IdentityField,
    subIdentityField: IdentityField
  ): Record<any, any>;

  filterReportSegments(
    visitorProperties: Record<string, any>
  ): Record<string, any>;

  isValidConfigData(data: ConfigData): boolean;
}
