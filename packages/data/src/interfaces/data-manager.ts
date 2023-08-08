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
  SegmentsData
} from '@convertcom/js-sdk-types';
import {DataStoreManagerInterface} from './data-store-manager';
import {GoalDataKey, RuleError} from '@convertcom/js-sdk-enums';

export interface DataManagerInterface {
  data: ConfigData;
  dataStoreManager: DataStoreManagerInterface;

  putLocalStore(storeKey: Id, storeData: StoreData);
  getLocalStore(storeKey: Id): StoreData;
  getStoreKey(visitorId: Id): string;
  selectLocations(
    visitorId: string,
    items: Array<Record<string, any>>,
    locationProperties: Record<string, any>
  ): Array<Record<string, any> | RuleError>;
  getBucketing(
    visitorId: Id,
    experienceKey: string,
    visitorProperties: Record<string, any> | null,
    locationProperties: Record<string, any> | null,
    environment?: string
  ): BucketedVariation | RuleError;
  getBucketingById(
    visitorId: Id,
    experienceId: Id,
    visitorProperties: Record<string, any> | null,
    locationProperties: Record<string, any> | null,
    environment?: string
  ): BucketedVariation | RuleError;
  convert(
    visitorId: Id,
    goalId: Id,
    goalRule?: Record<string, any>,
    goalData?: Array<Record<GoalDataKey, number>>,
    segments?: SegmentsData
  ): RuleError;
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

  isValidConfigData(data: ConfigData): boolean;
}
