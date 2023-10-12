/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {Entity} from '../types/Entity';
import {ConfigData} from '../types/Config';
import {Id} from '../types/Id';
import {IdentityField} from '../types/IndentityField';
import {BucketedVariation} from '../types/BucketedVariation';
import {DataStoreManagerInterface} from './data-store-manager';
import {StoreData} from '../types/StoreData';
import {SegmentsData} from '../types/SegmentsData';
import {GoalDataKey} from '../enums/goal-data-key';

export interface DataManagerInterface {
  data: ConfigData;
  dataStoreManager: DataStoreManagerInterface;

  putLocalStore(storeKey: Id, storeData: StoreData);
  getLocalStore(storeKey: Id): StoreData | null;
  getStoreKey(visitorId: Id): string;
  getBucketing(
    visitorId: Id,
    experienceKey: string,
    visitorProperties: Record<string, any>,
    locationProperties: Record<string, any>,
    environment?: string
  ): BucketedVariation | null;
  getBucketingById(
    visitorId: Id,
    experienceId: Id,
    visitorProperties: Record<string, any>,
    locationProperties: Record<string, any>,
    environment?: string
  ): BucketedVariation | null;
  convert(
    visitorId: Id,
    goalId: Id,
    goalRule?: Record<string, any>,
    goalData?: Array<Record<GoalDataKey, number>>,
    segments?: SegmentsData
  ): boolean;
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
