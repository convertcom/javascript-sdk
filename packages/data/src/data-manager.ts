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
} from '@convertcom/js-sdk-utils';

import {ApiManagerInterface} from '@convertcom/js-sdk-api';
import {BucketingManagerInterface} from '@convertcom/js-sdk-bucketing';
import {DataStoreManagerInterface} from './interfaces/data-store-manager';
import {DataManagerInterface} from './interfaces/data-manager';
import {EventManagerInterface} from '@convertcom/js-sdk-event';
import {LogManagerInterface} from '@convertcom/js-sdk-logger';
import {RuleManagerInterface} from '@convertcom/js-sdk-rules';
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
  SegmentsData,
  Segments
} from '@convertcom/js-sdk-types';

import {
  DATA_ENTITIES,
  ERROR_MESSAGES,
  MESSAGES,
  RuleError,
  EventType,
  GoalDataKey,
  SegmentsKeys,
  SystemEvents
} from '@convertcom/js-sdk-enums';

import {DataStoreManager} from './data-store-manager';
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
   * Validate locationProperties against locations rules and visitorProperties against audiences rules
   * @param {string} visitorId
   * @param {string|Id} identity Value of the field which name is provided in identityField
   * @param {Record<string, any> | null} visitorProperties
   * @param {Record<string, any> | null} locationProperties
   * @param {IdentityField=} identityField Defaults to 'key'
   * @param {string=} environment
   * @return {Experience | RuleError}
   */
  matchRulesByField(
    visitorId: string,
    identity: string | Id,
    visitorProperties: Record<string, any> | null,
    locationProperties: Record<string, any> | null,
    identityField: IdentityField = 'key',
    environment: string = this._environment
  ): Experience | RuleError {
    this._loggerManager?.trace?.('DataManager.matchRulesByField()', {
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
      (id) => String(experience?.id) === String(id)
    );
    // Check environment
    const isEnvironmentMatch = Array.isArray(experience?.environments)
      ? !experience.environments.length || // skip if empty
        experience.environments.includes(environment)
      : true; // skip if no environments

    let matchedErrors = [];
    if (experience && !isArchivedExperience && isEnvironmentMatch) {
      let locationMatched: boolean | RuleError = false,
        matchedLocations = [];
      if (locationProperties) {
        if (
          Array.isArray(experience?.locations) &&
          experience.locations.length
        ) {
          // Get attached locations
          const locations = this.getItemsByIds(
            experience.locations,
            'locations'
          ) as Array<Location>;
          if (locations.length) {
            // Validate locationProperties against locations rules
            // and trigger activated/deactivated events
            matchedLocations = this.selectLocations(
              visitorId,
              locations,
              locationProperties,
              identityField
            );
            // Return rule errors if present
            matchedErrors = matchedLocations.filter((match) =>
              Object.values(RuleError).includes(match as RuleError)
            );
            if (matchedErrors.length) return matchedErrors[0] as RuleError;
          }
          // If there are some matched locations
          locationMatched = Boolean(matchedLocations.length);
        } else if (experience?.site_area) {
          locationMatched = this._ruleManager.isRuleMatched(
            locationProperties,
            experience.site_area
          );
          // Return rule errors if present
          if (Object.values(RuleError).includes(locationMatched as RuleError))
            return locationMatched as RuleError;
        } else {
          // Empty experience locations list or unset Site Area means there's no restriction for the location
          locationMatched = true;
        }
      }
      // Validate locationProperties against site area rules
      if (!locationProperties || locationMatched) {
        let audiences = [],
          segmentations = [],
          matchedAudiences = [],
          matchedSegmentations = [];
        if (visitorProperties) {
          if (
            Array.isArray(experience?.audiences) &&
            experience.audiences.length
          ) {
            // Get attached transient and/or permnent audiences
            audiences = this.getItemsByIds(
              experience.audiences,
              'audiences'
            ) as Array<Audience>;
            if (audiences.length) {
              // Validate visitorProperties against audiences rules
              matchedAudiences = this.filterMatchedRecordsWithRule(
                audiences,
                visitorProperties
              );
              // Return rule errors if present
              matchedErrors = matchedAudiences.filter((match) =>
                Object.values(RuleError).includes(match as RuleError)
              );
              if (matchedErrors.length) return matchedErrors[0] as RuleError;
              if (matchedAudiences.length) {
                for (const item of matchedAudiences) {
                  this._loggerManager?.info?.(
                    MESSAGES.AUDIENCE_MATCH.replace('#', item?.id || item?.key)
                  );
                }
              }
            }
            // Get attached segmentation audiences
            segmentations = this.getItemsByIds(
              experience.audiences,
              'segments'
            ) as Array<Segments>;
            if (segmentations.length) {
              // Validate custom segments against segmentations
              matchedSegmentations = this.filterMatchedCustomSegments(
                segmentations,
                visitorId
              );
              if (matchedSegmentations.length) {
                for (const item of matchedSegmentations) {
                  this._loggerManager?.info?.(
                    MESSAGES.SEGMENTATION_MATCH.replace(
                      '#',
                      item?.id || item?.key
                    )
                  );
                }
              }
            }
          }
        }
        // If there are some matched audiences
        if (
          !visitorProperties ||
          matchedAudiences.length ||
          matchedSegmentations.length ||
          !audiences.length // Empty audiences list means there's no restriction for the audience
        ) {
          // And experience has variations
          if (experience?.variations && experience?.variations?.length) {
            return experience;
          } else {
            this._loggerManager?.info?.(MESSAGES.VARIATIONS_NOT_FOUND);
            this._loggerManager?.debug?.(MESSAGES.VARIATIONS_NOT_FOUND, {
              visitorProperties: visitorProperties,
              audiences: audiences
            });
          }
        } else {
          this._loggerManager?.info?.(MESSAGES.AUDIENCE_NOT_MATCH);
          this._loggerManager?.debug?.(MESSAGES.AUDIENCE_NOT_MATCH, {
            visitorProperties: visitorProperties,
            audiences: audiences
          });
        }
      } else {
        this._loggerManager?.info?.(MESSAGES.LOCATION_NOT_MATCH);
        this._loggerManager?.debug?.(MESSAGES.LOCATION_NOT_MATCH, {
          locationProperties: locationProperties,
          [experience?.locations
            ? 'experiences[].variations[].locations'
            : 'experiences[].variations[].site_area']:
            experience?.locations || experience?.site_area || ''
        });
      }
    } else {
      this._loggerManager?.info?.(MESSAGES.EXPERIENCE_NOT_FOUND);
      this._loggerManager?.debug?.(MESSAGES.EXPERIENCE_NOT_FOUND, {
        identity: identity,
        identityField: identityField
      });
    }
    return null;
  }

  /**
   * Retrieve variation for visitor
   * @param {string} visitorId
   * @param {string|Id} identity Value of the field which name is provided in identityField
   * @param {Record<string, any> | null} visitorProperties
   * @param {Record<string, any> | null} locationProperties
   * @param {IdentityField=} identityField Defaults to 'key'
   * @param {string=} environment
   * @return {BucketedVariation | RuleError}
   * @private
   */
  private _getBucketingByField(
    visitorId: string,
    identity: string | Id,
    visitorProperties: Record<string, any> | null,
    locationProperties: Record<string, any> | null,
    identityField: IdentityField = 'key',
    environment: string = this._environment
  ): BucketedVariation | RuleError {
    this._loggerManager?.trace?.('DataManager._getBucketingByField()', {
      visitorId: visitorId,
      identity: identity,
      visitorProperties: visitorProperties,
      locationProperties: locationProperties,
      identityField: identityField,
      environment: environment
    });

    // Retrieve the experience
    const experience = this.matchRulesByField(
      visitorId,
      identity,
      visitorProperties,
      locationProperties,
      identityField,
      environment
    );
    if (experience) {
      if (Object.values(RuleError).includes(experience as RuleError)) {
        return experience as RuleError;
      }
      return this._retrieveBucketing(visitorId, experience as Experience);
    }
    return null;
  }

  /**
   * Retrieve bucketing for Visitor
   * @param {Id} visitorId
   * @param {Experience} experience
   * @return {BucketedVariation}
   * @private
   */
  private _retrieveBucketing(
    visitorId: Id,
    experience: Experience
  ): BucketedVariation {
    if (!visitorId || !experience) return null;
    if (!experience?.id) return null;
    let variation = null;
    let bucketedVariation = null;
    const storeKey = this.getStoreKey(visitorId);
    // Check that visitor id already bucketed and stored and skip bucketing logic
    const {bucketing, locations, segments} =
      this.getLocalStore(visitorId) || {};
    const {[experience.id.toString()]: variationId} = bucketing || {};
    if (
      variationId &&
      (variation = this.retrieveVariation(experience.id, variationId))
    ) {
      // If it's found log debug info. The return value will be formed next step
      this._loggerManager?.info?.(
        MESSAGES.BUCKETED_VISITOR_FOUND.replace('#', `#${variationId}`)
      );
      this._loggerManager?.debug?.({
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
          bucketing: {...bucketing, [experience.id.toString()]: variationId},
          ...(locations ? {locations} : {}),
          ...(segments ? {segments} : {})
        });
        // If it's found log debug info. The return value will be formed next step
        this._loggerManager?.info?.(
          MESSAGES.BUCKETED_VISITOR_FOUND.replace('#', `#${variationId}`)
        );
        this._loggerManager?.debug?.({
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
          this._loggerManager?.info?.(
            MESSAGES.BUCKETED_VISITOR.replace('#', `#${variationId}`)
          );
          // Store the data in local variable
          const storeData: StoreData = {
            bucketing: {...bucketing, [experience.id.toString()]: variationId},
            ...(locations ? {locations} : {}),
            ...(segments ? {segments} : {})
          };
          this.putLocalStore(visitorId, storeData);
          // Enqueue to store in dataStore
          this.dataStoreManager.enqueue(storeKey, storeData);
          // Enqueue bucketing event to api
          const bucketingEvent: BucketingEvent = {
            experienceId: experience.id.toString(),
            variationId: variationId.toString()
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
   * @return {StoreData} variation id
   * @private
   */
  getLocalStore(visitorId: Id): StoreData {
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
   *
   * @param {string} visitorId
   * @param {Array<Record<string, any>>} items
   * @param {Record<string, any>} locationProperties
   * @returns {Array<Record<string, any> | RuleError>}
   */
  selectLocations(
    visitorId: string,
    items: Array<Record<string, any>>,
    locationProperties: Record<string, any>,
    identityField: IdentityField = 'key'
  ): Array<Record<string, any> | RuleError> {
    this._loggerManager?.trace?.('DataManager.selectLocations()', {
      items: items,
      locationProperties: locationProperties
    });
    // Get locations from DataStore
    const storeData = this.getLocalStore(visitorId) || {};
    const {bucketing, locations = [], segments} = storeData;
    const matchedRecords = [];
    let match;
    if (arrayNotEmpty(items)) {
      for (let i = 0, length = items.length; i < length; i++) {
        if (!items?.[i]?.rules) continue;
        match = this._ruleManager.isRuleMatched(
          locationProperties,
          items[i].rules
        );
        const identity = items?.[i]?.[identityField]?.toString?.();
        if (match === true) {
          this._loggerManager?.info?.(
            MESSAGES.LOCATION_MATCH.replace('#', `#${identity}`)
          );
          if (!locations.includes(identity)) {
            locations.push(identity);
            this._eventManager.fire(
              SystemEvents.LOCATION_ACTIVATED,
              {
                visitorId,
                location: {
                  id: items?.[i]?.id,
                  key: items?.[i]?.key,
                  name: items?.[i]?.name
                }
              },
              null,
              true
            );
            this._loggerManager?.info?.(
              MESSAGES.LOCATION_ACTIVATED.replace('#', `#${identity}`)
            );
          }
          matchedRecords.push(items[i]);
        } else if (match !== false) {
          // catch rule errors
          matchedRecords.push(match);
        } else if (match === false && locations.includes(identity)) {
          this._eventManager.fire(
            SystemEvents.LOCATION_DEACTIVATED,
            {
              visitorId,
              location: {
                id: items?.[i]?.id,
                key: items?.[i]?.key,
                name: items?.[i]?.name
              }
            },
            null,
            true
          );
          const locationIndex = locations.findIndex(
            (location) => location === identity
          );
          locations.splice(locationIndex, 1);
          this._loggerManager?.info?.(
            MESSAGES.LOCATION_DEACTIVATED.replace('#', `#${identity}`)
          );
        }
      }
    }
    // Store the data in local variable
    this.putLocalStore(visitorId, {
      ...(bucketing ? {bucketing} : {}),
      locations,
      ...(segments ? {segments} : {})
    });
    this._loggerManager?.debug?.('DataManager.selectLocations()', {
      matchedRecords: matchedRecords
    });
    return matchedRecords;
  }

  /**
   * Retrieve variation for visitor
   * @param {string} visitorId
   * @param {string} key
   * @param {Record<string, any> | null} visitorProperties
   * @param {Record<string, any> | null} locationProperties
   * @param {string=} environment
   * @return {BucketedVariation | RuleError}
   */
  getBucketing(
    visitorId: string,
    key: string,
    visitorProperties: Record<string, any> | null,
    locationProperties: Record<string, any> | null,
    environment: string = this._environment
  ): BucketedVariation | RuleError {
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
   * @param {Record<string, any> | null} visitorProperties
   * @param {Record<string, any> | null} locationProperties
   * @param {string=} environment
   * @return {BucketedVariation | RuleError}
   */
  getBucketingById(
    visitorId: string,
    id: Id,
    visitorProperties: Record<string, any> | null,
    locationProperties: Record<string, any> | null,
    environment: string = this._environment
  ): BucketedVariation | RuleError {
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
  ): RuleError {
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
      const ruleMatched = this._ruleManager.isRuleMatched(goalRule, goal.rules);
      // Return rule errors if present
      if (Object.values(RuleError).includes(ruleMatched as RuleError))
        return ruleMatched as RuleError;
      if (!ruleMatched) {
        this._loggerManager?.error?.(MESSAGES.GOAL_RULE_NOT_MATCH);
        return;
      }
    }
    const data: ConversionEvent = {
      goalId: goal.id
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
        goalId: goal.id,
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
   * @param {Record<string, any>} visitorProperties
   * @return {Array<Record<string, any> | RuleError>}
   */
  filterMatchedRecordsWithRule(
    items: Array<Record<string, any>>,
    visitorProperties: Record<string, any>
  ): Array<Record<string, any> | RuleError> {
    this._loggerManager?.trace?.('DataManager.filterMatchedRecordsWithRule()', {
      items: items,
      visitorProperties: visitorProperties
    });
    const matchedRecords = [];
    let match;
    if (arrayNotEmpty(items)) {
      for (let i = 0, length = items.length; i < length; i++) {
        if (!items?.[i]?.rules) continue;
        match = this._ruleManager.isRuleMatched(
          visitorProperties,
          items[i].rules
        );
        if (match === true) {
          matchedRecords.push(items[i]);
        } else if (match !== false) {
          // catch rule errors
          matchedRecords.push(match);
        }
      }
    }
    this._loggerManager?.debug?.('DataManager.filterMatchedRecordsWithRule()', {
      matchedRecords: matchedRecords
    });
    return matchedRecords;
  }

  /**
   * Get audiences that meet the custom segments
   * @param {Array<Record<any, any>>} items
   * @param {Id} visitorId
   * @return {Array<Record<string, any>>}
   */
  filterMatchedCustomSegments(
    items: Array<Record<string, any>>,
    visitorId: Id
  ): Array<Record<string, any>> {
    this._loggerManager?.trace?.('DataManager.filterMatchedCustomSegments()', {
      items: items,
      visitorId: visitorId
    });
    // Check that custom segments are matched
    const storeData = this.getLocalStore(visitorId) || {};
    // Get custom segments ID from DataStore
    const {
      segments: {[SegmentsKeys.CUSTOM_SEGMENTS]: customSegments = []} = {}
    } = storeData;
    const matchedRecords = [];
    if (arrayNotEmpty(items)) {
      for (let i = 0, length = items.length; i < length; i++) {
        if (!items?.[i]?.id) continue;
        if (customSegments.includes(items[i].id)) {
          matchedRecords.push(items[i]);
        }
      }
    }
    this._loggerManager?.debug?.('DataManager.filterMatchedCustomSegments()', {
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
        if (list[i] && String(list[i]?.[identityField]) === String(identity)) {
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
   * @return {Entity}
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
   * @return {Entity}
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
          if (
            ids.indexOf(Number(list[i]?.id)) !== -1 ||
            ids.indexOf(String(list[i]?.id)) !== -1
          ) {
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
   * @return {Record<any, any>}
   */
  getSubItem(
    entityType: string,
    entityIdentity: string | number,
    subEntityType: string,
    subEntityIdentity: string | number,
    identityField: IdentityField,
    subIdentityField: IdentityField
  ): Record<any, any> {
    const entity = this._getEntityByField(
      entityIdentity,
      entityType,
      identityField
    );
    for (const k in entity[subEntityType]) {
      if (
        String(entity[subEntityType][k]?.[subIdentityField]) ===
        String(subEntityIdentity)
      ) {
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
