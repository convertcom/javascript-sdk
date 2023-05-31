/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {
  arrayNotEmpty,
  objectDeepValue,
  objectNotEmpty
} from '@convertcom/utils';

import {ApiManagerInterface} from './interfaces/api-manager';
import {BucketingManagerInterface} from '@convertcom/bucketing';
import {DataStoreManagerInterface} from '@convertcom/datastore';
import {DataManagerInterface} from './interfaces/data-manager';
import {EventManagerInterface} from '@convertcom/event';
import {LogManagerInterface} from '@convertcom/logger';
import {RuleManagerInterface} from '@convertcom/rules';
import {
  Entity,
  Variation,
  Id,
  Audience,
  Location,
  Config,
  ConfigData,
  Experience,
  IdentityField,
  BucketedVariation,
  StoreData,
  BucketingEvent,
  VisitorEvent,
  ConversionEvent,
  Goal,
  SegmentsData
} from '@convertcom/types';

import {
  DATA_ENTITIES,
  ERROR_MESSAGES,
  MESSAGES,
  EventType,
  GoalDataKey
} from '@convertcom/enums';

import {DataStoreManager} from '@convertcom/datastore';
const LOCAL_STORE_LIMIT = 10000;
/**
 * Provides logic for data. Stores bucket with help of dataStore if it's provided
 * @category Modules
 * @constructor
 * @implements {DataManagerInterface}
 */
export class DataManager implements DataManagerInterface {
  private _data: ConfigData;
  private _accountId: Id;
  private _projectId: Id;
  private _config: Config;
  private _bucketingManager: BucketingManagerInterface;
  private _loggerManager: LogManagerInterface;
  private _eventManager: EventManagerInterface;
  private _dataStoreManager: DataStoreManagerInterface;
  private _apiManager: ApiManagerInterface;
  private _ruleManager: RuleManagerInterface;
  private _dataEntities = DATA_ENTITIES;
  private _localStoreLimit = LOCAL_STORE_LIMIT;
  private _bucketedVisitors = new Map();
  private _environment: string;
  /**
   * @param {Config} config
   * @param {Object} dependencies
   * @param {ApiManagerInterface} dependencies.apiManager
   * @param {BucketingManagerInterface} dependencies.bucketingManager
   * @param {RuleManagerInterface} dependencies.ruleManager
   * @param {LogManagerInterface} dependencies.loggerManager
   */
  constructor(
    config: Config,
    {
      bucketingManager,
      ruleManager,
      eventManager,
      apiManager,
      loggerManager
    }: {
      bucketingManager: BucketingManagerInterface;
      ruleManager: RuleManagerInterface;
      eventManager: EventManagerInterface;
      apiManager: ApiManagerInterface;
      loggerManager?: LogManagerInterface;
    }
  ) {
    this._environment = config?.environment;
    this._apiManager = apiManager;
    this._bucketingManager = bucketingManager;
    this._ruleManager = ruleManager;
    this._loggerManager = loggerManager;
    this._eventManager = eventManager;
    this._config = config;
    this._data = objectDeepValue(config, 'data');
    this._accountId = this._data?.account_id;
    this._projectId = this._data?.project?.id;
    this.dataStoreManager = objectDeepValue(config, 'dataStore');
    this._loggerManager?.trace?.(MESSAGES.DATA_CONSTRUCTOR, this);
  }

  set data(data: ConfigData) {
    if (this.isValidConfigData(data)) {
      this._data = data;
      this._accountId = data?.account_id;
      this._projectId = data?.project?.id;
    } else {
      this._loggerManager?.error?.(ERROR_MESSAGES.CONFIG_DATA_NOT_VALID);
    }
  }

  /**
   * data getter
   */
  get data(): ConfigData {
    return this._data;
  }

  /**
   * dataStoreManager setter
   * @param {any=} dataStore
   */
  set dataStoreManager(dataStore: any) {
    this._dataStoreManager = null;
    this._dataStoreManager = new DataStoreManager(this._config, {
      dataStore: dataStore,
      eventManager: this._eventManager,
      loggerManager: this._loggerManager
    });
  }

  /**
   * dataStoreManager getter
   */
  get dataStoreManager(): DataStoreManagerInterface {
    return this._dataStoreManager;
  }

  /**
   * Retrieve variation for visitor
   * @param {string} visitorId
   * @param {string|Id} identity Value of the field which name is provided in identityField
   * @param {Record<string, any>} visitorProperties
   * @param {string} locationProperties
   * @param {IdentityField=} identityField Defaults to 'key'
   * @param {string=} environment
   * @return {BucketedVariation|null}
   * @private
   */
  private _getBucketingByField(
    visitorId: string,
    identity: string | Id,
    visitorProperties: Record<string, any>,
    locationProperties: Record<string, any>,
    identityField: IdentityField = 'key',
    environment: string = this._environment
  ): BucketedVariation | null {
    this._loggerManager?.trace?.('DataManager._getBucketingByField()', {
      visitorId: visitorId,
      identity: identity,
      visitorProperties: visitorProperties,
      locationProperties: locationProperties,
      identityField: identityField,
      environment: environment
    });
    // Retrieve the experience
    const experience = this._getEntityByField(
      identity,
      'experiences',
      identityField
    ) as Experience;
    // Retrieve archived experiences
    const archivedExperiences = this.getEntitiesList(
      'archived_experiences'
    ) as Array<Id>;
    // Check whether the experience is archived
    const isArchivedExperience = !!archivedExperiences.find(
      (id) => experience?.id == id
    );

    if (
      experience &&
      !isArchivedExperience &&
      experience.environments.includes(environment)
    ) {
      let locationMatched = false;
      if (experience?.locations) {
        // Get attached locations
        const locations = this.getItemsByIds(
          experience?.locations,
          'locations'
        ) as Array<Location>;
        // Validate locationProperties against locations rules
        const matchedLocations = this.filterMatchedRecordsWithRule(
          locations,
          locationProperties
        );
        // If there are some matched locations
        locationMatched = Boolean(
          !locationProperties || matchedLocations.length
        );
      } else if (experience?.site_area) {
        locationMatched = this._ruleManager.isRuleMatched(
          locationProperties,
          experience?.site_area
        );
      }
      // Validate locationProperties against site area rules
      if (!locationProperties || locationMatched) {
        let audiences,
          matchedAudiences = [];
        if (experience?.audiences) {
          // Get attached audiences
          audiences = this.getItemsByIds(
            experience?.audiences,
            'audiences'
          ) as Array<Audience>;
          // Validate visitorProperties against audiences rules
          matchedAudiences = this.filterMatchedRecordsWithRule(
            audiences,
            visitorProperties
          );
        }
        // If there are some matched audiences
        if (!visitorProperties || matchedAudiences.length) {
          // And experience has variations
          if (experience?.variations && experience?.variations?.length) {
            return this._retrieveBucketing(visitorId, experience);
          } else {
            this._loggerManager?.debug?.(MESSAGES.VARIATIONS_NOT_FOUND, {
              visitorProperties: visitorProperties,
              audiences: audiences
            });
          }
        } else {
          this._loggerManager?.debug?.(MESSAGES.RULES_NOT_MATCH, {
            visitorProperties: visitorProperties,
            audiences: audiences
          });
        }
      } else {
        this._loggerManager?.debug?.(MESSAGES.LOCATION_NOT_MATCH, {
          locationProperties: locationProperties,
          [experience?.locations
            ? 'experiences[].variations[].locations'
            : 'experiences[].variations[].site_area']:
            experience?.locations || experience?.site_area || ''
        });
      }
    }
    return null;
  }

  /**
   * Retrieve bucketing for Visitor
   * @param {Id} visitorId
   * @param {Experience} experience
   * @return {BucketedVariation | null}
   * @private
   */
  private _retrieveBucketing(
    visitorId: Id,
    experience: Experience
  ): BucketedVariation | null {
    if (!visitorId || !experience) return null;
    if (!experience?.id) return null;
    let variation = null;
    let bucketedVariation = null;
    const storeKey = this.getStoreKey(visitorId);
    // Check that visitor id already bucketed and stored and skip bucketing logic
    const {
      bucketing: {[experience.id.toString()]: variationId} = {},
      segments
    } = this.getLocalStore(visitorId) || {};
    if (
      variationId &&
      (variation = this.retrieveVariation(experience.id, variationId))
    ) {
      // If it's found log debug info. The return value will be formed next step
      this._loggerManager?.debug?.(MESSAGES.BUCKETED_VISITOR_FOUND, {
        storeKey: storeKey,
        visitorId: visitorId,
        variationId: variationId
      });
    } else {
      // Try to find a bucketed visitor in dataStore
      let {bucketing: {[experience.id.toString()]: variationId} = {}} =
        this.dataStoreManager?.get?.(storeKey) || {};
      if (
        variationId &&
        (variation = this.retrieveVariation(experience.id, variationId))
      ) {
        // Store the data in local variable
        this.putLocalStore(visitorId, {
          bucketing: {[experience.id.toString()]: variationId},
          ...(segments ? {segments} : {})
        });
        // If it's found log debug info. The return value will be formed next step
        this._loggerManager?.debug?.(MESSAGES.BUCKETED_VISITOR_FOUND, {
          storeKey: storeKey,
          visitorId: visitorId,
          variationId: variationId
        });
      } else {
        // Build buckets where key is variation id and value is traffic distribution
        const buckets = experience.variations.reduce((bucket, variation) => {
          if (variation?.id)
            bucket[variation.id] = variation?.traffic_allocation || 100.0;
          return bucket;
        }, {});
        // Select bucket based for provided visitor id
        variationId = this._bucketingManager.getBucketForVisitor(
          buckets,
          visitorId
        ) as Id;
        if (variationId) {
          // Store the data in local variable
          const storeData: StoreData = {
            bucketing: {[experience.id.toString()]: variationId},
            ...(segments ? {segments} : {})
          };
          this.putLocalStore(visitorId, storeData);
          // Enqueue to store in dataStore
          this.dataStoreManager.enqueue(storeKey, storeData);
          // Enqueue bucketing event to api
          const bucketingEvent: BucketingEvent = {
            experienceId: experience.id,
            variationId
          };
          const visitorEvent: VisitorEvent = {
            eventType: EventType.BUCKETING,
            data: bucketingEvent
          };
          this._apiManager.enqueue(visitorId, visitorEvent, segments);
          this._loggerManager?.trace?.('DataManager._retrieveBucketing()', {
            visitorEvent
          });
          // Retrieve and return variation
          variation = this.retrieveVariation(experience.id, variationId);
        } else {
          this._loggerManager?.error?.(
            ERROR_MESSAGES.UNABLE_TO_SELECT_BUCKET_FOR_VISITOR,
            {
              visitorId: visitorId,
              experience: experience
            }
          );
        }
      }
    }

    // Build the response as bucketed variation object
    if (variation) {
      bucketedVariation = {
        ...{
          experienceId: experience?.id,
          experienceName: experience?.name,
          experienceKey: experience?.key
        },
        ...variation
      };
    }

    return bucketedVariation as BucketedVariation;
  }

  /**
   * @param {Id} experienceId
   * @param {Id} variationId
   * @return {Variation}
   * @private
   */
  private retrieveVariation(experienceId: Id, variationId: Id): Variation {
    return this.getSubItem(
      'experiences',
      experienceId,
      'variations',
      variationId,
      'id',
      'id'
    ) as Variation;
  }

  /**
   * @param {Id} visitorId
   * @param {StoreData} storeData
   * @private
   */
  putLocalStore(visitorId: Id, storeData: StoreData) {
    const storeKey = this.getStoreKey(visitorId);
    this._bucketedVisitors.set(storeKey, storeData);
    if (this._bucketedVisitors.size > this._localStoreLimit) {
      // Delete one of the oldest record
      for (const [key, value] of this._bucketedVisitors) {
        this._bucketedVisitors.delete(key);
        break;
      }
    }
  }

  /**
   * @param {Id} visitorId
   * @return {StoreData | null} variation id
   * @private
   */
  getLocalStore(visitorId: Id): StoreData | null {
    const storeKey = this.getStoreKey(visitorId);
    return this._bucketedVisitors.get(storeKey) || null;
  }

  /**
   * @param {Id} visitorId
   * @return {string} storeKey
   * @private
   */
  getStoreKey(visitorId: Id): string {
    return `${this._accountId}-${this._projectId}-${visitorId}`;
  }

  /**
   * Retrieve variation for visitor
   * @param {string} visitorId
   * @param {string} key
   * @param {Record<string, any>} visitorProperties
   * @param {string} locationProperties
   * @param {string=} environment
   * @return {BucketedVariation | null}
   */
  getBucketing(
    visitorId: string,
    key: string,
    visitorProperties: Record<string, any>,
    locationProperties: Record<string, any>,
    environment: string = this._environment
  ): BucketedVariation | null {
    return this._getBucketingByField(
      visitorId,
      key,
      visitorProperties,
      locationProperties,
      'key',
      environment
    );
  }

  /**
   * Retrieve variation for Visitor
   * @param {string} visitorId
   * @param {Id} id
   * @param {Record<string, any>} visitorProperties
   * @param {string} locationProperties
   * @param {string=} environment
   * @return {BucketedVariation | null}
   */
  getBucketingById(
    visitorId: string,
    id: Id,
    visitorProperties: Record<string, any>,
    locationProperties: Record<string, any>,
    environment: string = this._environment
  ): BucketedVariation | null {
    return this._getBucketingByField(
      visitorId,
      id,
      visitorProperties,
      locationProperties,
      'id',
      environment
    );
  }

  /**
   * Process conversion event
   * @param {Id} visitorId
   * @param {Id} goalId
   * @param {Record<string, any>=} goalRule An object of key-value pairs that are used for goal matching
   * @param {Array<Record<GoalDataKey, number>>} goalData An array of object of key-value pairs
   * @param {SegmentsData} segments
   */
  convert(
    visitorId: Id,
    goalId: Id,
    goalRule?: Record<string, any>,
    goalData?: Array<Record<GoalDataKey, number>>,
    segments?: SegmentsData
  ): void {
    const goal =
      typeof goalId === 'string'
        ? (this.getEntity(goalId as string, 'goals') as Goal)
        : (this.getEntityById(goalId, 'goals') as Goal);
    if (!goal?.id) {
      this._loggerManager?.error?.(MESSAGES.GOAL_NOT_FOUND);
      return;
    }

    if (goalRule) {
      if (!goal?.rules) return;
      const ruleMatched = this._ruleManager.isRuleMatched(
        goalRule,
        goal?.rules
      );
      if (!ruleMatched) {
        this._loggerManager?.error?.(MESSAGES.GOAL_RULE_NOT_MATCH);
        return;
      }
    }
    const data: ConversionEvent = {
      goalId: goal?.id
    };
    const {bucketing: bucketingData} = this.getLocalStore(visitorId) || {};
    if (bucketingData) data.bucketingData = bucketingData;
    const event: VisitorEvent = {
      eventType: EventType.CONVERSION,
      data
    };
    this._apiManager.enqueue(visitorId, event, segments);
    // Split transaction events
    if (goalData) {
      const data: ConversionEvent = {
        goalId: goal?.id,
        goalData
      };
      if (bucketingData) data.bucketingData = bucketingData;
      const event: VisitorEvent = {
        eventType: EventType.CONVERSION,
        data
      };
      this._apiManager.enqueue(visitorId, event, segments);
    }
    this._loggerManager?.trace?.('DataManager.convert()', {
      event
    });
  }

  /**
   * Get audiences that meet the visitorProperties
   * @param {Array<Record<any, any>>} items
   * @param {Object} visitorProperties
   * @return {Array<Record<string, any>>}
   */
  filterMatchedRecordsWithRule(
    items: Array<Record<string, any>>,
    visitorProperties: Record<string, any>
  ): Array<Record<string, any>> {
    this._loggerManager?.trace?.('DataManager.filterMatchedRecordsWithRule()', {
      items: items,
      visitorProperties: visitorProperties
    });
    const matchedRecords = [];
    if (arrayNotEmpty(items)) {
      for (let i = 0, length = items.length; i < length; i++) {
        if (!items?.[i]?.rules) continue;
        if (
          this._ruleManager.isRuleMatched(visitorProperties, items?.[i]?.rules)
        ) {
          matchedRecords.push(items[i]);
        }
      }
    }
    this._loggerManager?.debug?.('DataManager.filterMatchedRecordsWithRule()', {
      matchedRecords: matchedRecords
    });
    return matchedRecords;
  }

  /**
   * Get list of data entities
   * @param {string} entityType
   * @return {Array<Entity | Id>}
   */
  getEntitiesList(entityType: string): Array<Entity | Id> {
    let list = [];
    if (this._dataEntities.indexOf(entityType) !== -1) {
      list = objectDeepValue(this._data, entityType) || [];
    }
    this._loggerManager?.trace?.('DataManager.getEntitiesList()', {
      entityType: entityType,
      list: list
    });
    return list;
  }

  /**
   * Get list of data entities grouped by field
   * @param {string} entityType
   * @param {IdentityField=} field
   * @return {Record<string, Entity>}
   */
  getEntitiesListObject(
    entityType: string,
    field: IdentityField = 'id'
  ): Record<string, Entity> {
    return this.getEntitiesList(entityType).reduce((target, entity) => {
      target[entity[field]] = entity;
      return target;
    }, {});
  }

  /**
   *
   * @param {string|Id} identity Value of the field which name is provided in identityField
   * @param {string} entityType
   * @param {IdentityField=} identityField Defaults to 'key'
   * @return {Entity}
   * @private
   */
  private _getEntityByField(
    identity: string | Id,
    entityType: string,
    identityField: IdentityField = 'key'
  ): Entity {
    this._loggerManager?.trace?.('DataManager._getEntityByField()', {
      identity: identity,
      entityType: entityType,
      identityField: identityField
    });
    const list = this.getEntitiesList(entityType) as Array<Entity>;
    if (arrayNotEmpty(list)) {
      for (let i = 0, length = list.length; i < length; i++) {
        if (list[i] && list[i]?.[identityField] === identity) {
          return list[i];
        }
      }
    }
    return null;
  }

  /**
   * Find the entity in list by id
   * @param {string} key
   * @param {string} entityType
   * @return {Entity|null}
   */
  getEntity(key: string, entityType: string): Entity {
    return this._getEntityByField(key, entityType, 'key');
  }

  /**
   * Find the entity in list by keys
   * @param {Array<string>} keys
   * @param {string} entityType
   * @return {Array<Entity>}
   */
  getEntities(keys: Array<string>, entityType: string): Array<Entity> {
    return this.getItemsByKeys(keys, entityType) as Array<Entity>;
  }

  /**
   * Find the entity in list by id
   * @param {Id} id
   * @param {string} entityType
   * @return {Entity|null}
   */
  getEntityById(id: Id, entityType: string): Entity {
    return this._getEntityByField(id, entityType, 'id');
  }

  /**
   * Find the entity in list by ids
   * @param {Array<Id>} ids
   * @param {string} entityType
   * @return {Array<Entity>}
   */
  getEntitiesByIds(ids: Array<Id>, entityType: string): Array<Entity> {
    return this.getItemsByIds(ids, entityType) as Array<Entity>;
  }

  /**
   * Find the items in list by  keys
   * @param {Array<string>} keys
   * @param {string} path
   * @return {Array<Record<string, any>>}
   */
  getItemsByKeys(
    keys: Array<string>,
    path: string
  ): Array<Record<string, any>> {
    const list = this.getEntitiesList(path) as Array<Entity>;
    const items = [];
    if (arrayNotEmpty(list)) {
      for (let i = 0, length = list.length; i < length; i++) {
        if (keys.indexOf(list[i]?.key) !== -1) {
          items.push(list[i]);
        }
      }
    }
    return items;
  }

  /**
   * Find the items in list by ids
   * @param {Array<Id>} ids
   * @param {String} path
   * @return {Array<Record<string, any>>}
   */
  getItemsByIds(ids: Array<Id>, path: string): Array<Record<string, any>> {
    this._loggerManager?.trace?.('DataManager.getItemsByIds()', {
      ids: ids,
      path: path
    });
    const items = [];
    if (arrayNotEmpty(ids)) {
      const list = this.getEntitiesList(path) as Array<Entity>;
      if (arrayNotEmpty(list)) {
        for (let i = 0, length = list.length; i < length; i++) {
          if (ids.indexOf(list[i]?.id) !== -1) {
            items.push(list[i]);
          }
        }
      }
    }
    return items;
  }

  /**
   * Find nested item
   * @param {string} entityType
   * @param {string|number} entityIdentity
   * @param {string} subEntityType
   * @param {string|number} subEntityIdentity
   * @param {IdentityField} identityField
   * @param {IdentityField} subIdentityField
   * @return {Record<any, any> | null}
   */
  getSubItem(
    entityType: string,
    entityIdentity: string | number,
    subEntityType: string,
    subEntityIdentity: string | number,
    identityField: IdentityField,
    subIdentityField: IdentityField
  ): Record<any, any> | null {
    const entity = this._getEntityByField(
      entityIdentity,
      entityType,
      identityField
    );
    for (const k in entity[subEntityType]) {
      if (entity[subEntityType][k]?.[subIdentityField] == subEntityIdentity) {
        return entity[subEntityType][k];
      }
    }
    return null;
  }

  /**
   * Validates data object
   * @param data
   * @return {boolean}
   */
  isValidConfigData(data: ConfigData): boolean {
    return objectNotEmpty(data) && !!data?.account_id && !!data?.project?.id;
  }
}
