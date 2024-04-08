/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {
  arrayNotEmpty,
  camelCase,
  objectDeepEqual,
  objectDeepMerge,
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
  ExperienceVariationConfig,
  ConfigAudience,
  ConfigLocation,
  Config,
  ConfigResponseData,
  ConfigExperience,
  IdentityField,
  BucketedVariation,
  StoreData,
  BucketingEvent,
  VisitorTrackingEvents,
  ConversionEvent,
  ConfigGoal,
  VisitorSegments,
  ConfigSegment,
  BucketingAttributes,
  ConfigAudienceTypes
} from '@convertcom/js-sdk-types';

import {
  DATA_ENTITIES,
  DATA_ENTITIES_MAP,
  ERROR_MESSAGES,
  MESSAGES,
  RuleError,
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
  private _data: ConfigResponseData;
  private _accountId: string;
  private _projectId: string;
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
  private _asyncStorage: boolean;
  private _environment: string;
  private _mapper: (...args: any) => any;
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
    },
    {
      asyncStorage = true
    }: {
      asyncStorage?: boolean;
    } = {}
  ) {
    this._environment = config?.environment;
    this._apiManager = apiManager;
    this._bucketingManager = bucketingManager;
    this._ruleManager = ruleManager;
    this._loggerManager = loggerManager;
    this._eventManager = eventManager;
    this._config = config;
    this._mapper = config?.mapper || ((value: any) => value);
    this._asyncStorage = asyncStorage;
    this._data = objectDeepValue(config, 'data');
    this._accountId = this._data?.account_id;
    this._projectId = this._data?.project?.id;
    this.dataStoreManager = config?.dataStore;
    this._loggerManager?.trace?.(
      'DataManager()',
      MESSAGES.DATA_CONSTRUCTOR,
      this
    );
  }

  set data(data: ConfigResponseData) {
    if (this.isValidConfigData(data)) {
      this._data = data;
      this._accountId = data?.account_id;
      this._projectId = data?.project?.id;
    } else {
      this._loggerManager?.error?.(
        'DataManager.data.set()',
        ERROR_MESSAGES.CONFIG_DATA_NOT_VALID
      );
    }
  }

  /**
   * data getter
   */
  get data(): ConfigResponseData {
    return this._data;
  }

  /**
   * dataStoreManager setter
   * @param {any=} dataStore
   */
  set dataStoreManager(dataStore: any) {
    this._dataStoreManager = null;
    if (dataStore) {
      this._dataStoreManager = new DataStoreManager(this._config, {
        dataStore: dataStore,
        eventManager: this._eventManager,
        loggerManager: this._loggerManager
      });
    }
  }

  /**
   * dataStoreManager getter
   */
  get dataStoreManager(): DataStoreManagerInterface {
    return this._dataStoreManager;
  }

  /**
   * Set dataStoreManager at run-time
   */
  setDataStore(dataStore: any) {
    this._dataStoreManager = null;
    if (dataStore) {
      this._dataStoreManager = new DataStoreManager(this._config, {
        dataStore: dataStore,
        eventManager: this._eventManager,
        loggerManager: this._loggerManager
      });
    }
  }

  /**
   * Validate locationProperties against locations rules and visitorProperties against audiences rules
   * @param {string} visitorId
   * @param {string} identity Value of the field which name is provided in identityField
   * @param {IdentityField=} identityField Defaults to 'key'
   * @param {BucketingAttributes} attributes
   * @param {Record<any, any>} attributes.locationProperties
   * @param {Record<any, any>} attributes.visitorProperties
   * @param {string=} attributes.environment
   * @return {ConfigExperience | RuleError}
   */
  matchRulesByField(
    visitorId: string,
    identity: string,
    identityField: IdentityField = 'key',
    attributes: BucketingAttributes
  ): ConfigExperience | RuleError {
    const {
      visitorProperties,
      locationProperties,
      environment = this._environment
    } = attributes;
    this._loggerManager?.trace?.(
      'DataManager.matchRulesByField()',
      this._mapper({
        visitorId: visitorId,
        identity: identity,
        identityField: identityField,
        visitorProperties: visitorProperties,
        locationProperties: locationProperties,
        environment: environment
      })
    );
    // Retrieve the experience
    const experience = this._getEntityByField(
      identity,
      'experiences',
      identityField
    ) as ConfigExperience;
    // Retrieve archived experiences
    const archivedExperiences = this.getEntitiesList(
      'archived_experiences'
    ) as Array<string>;
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
      // Check that visitor id already bucketed and stored and skip bucketing logic
      const {bucketing} = this.getData(visitorId) || {};
      const {[experience.id.toString()]: variationId} = bucketing || {};
      let isBucketed = false;
      if (
        variationId &&
        this.retrieveVariation(experience.id, String(variationId))
      ) {
        isBucketed = true;
      }

      // Check location rules against locationProperties
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
          ) as Array<ConfigLocation>;
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
          // Validate locationProperties against site area rules
          locationMatched = this._ruleManager.isRuleMatched(
            locationProperties,
            experience.site_area,
            'SiteArea'
          );
          // Return rule errors if present
          if (Object.values(RuleError).includes(locationMatched as RuleError))
            return locationMatched as RuleError;
        } else {
          locationMatched = true; // Empty experience locations list means no restrictions
          this._loggerManager?.info?.(
            'DataManager.matchRulesByField()',
            MESSAGES.LOCATION_NOT_RESTRICTED
          );
        }
      }
      if (!locationMatched) {
        this._loggerManager?.debug?.(
          'DataManager.matchRulesByField()',
          MESSAGES.LOCATION_NOT_MATCH,
          this._mapper({
            locationProperties: locationProperties,
            [experience?.locations
              ? 'experiences[].variations[].locations'
              : 'experiences[].variations[].site_area']:
              experience?.locations || experience?.site_area || ''
          })
        );
        return null;
      }

      // Check audience rules against visitorProperties
      let audiences = [],
        segments = [],
        matchedAudiences = [],
        matchedSegments = [],
        audiencesToCheck: Array<ConfigAudience> = [],
        audiencesMatched = false,
        segmentsMatched = false;
      if (visitorProperties) {
        if (
          Array.isArray(experience?.audiences) &&
          experience.audiences.length
        ) {
          // Get attached transient and/or permnent audiences
          audiences = this.getItemsByIds(
            experience.audiences,
            'audiences'
          ) as Array<ConfigAudience>;

          // If visitor already bucketed into this experience, check only audiences of type transient
          audiencesToCheck = audiences.filter(
            (audience) =>
              !(isBucketed && audience.type === ConfigAudienceTypes.PERMANENT)
          );
          if (audiencesToCheck.length) {
            // Validate visitorProperties against audiences rules
            matchedAudiences = this.filterMatchedRecordsWithRule(
              audiencesToCheck,
              visitorProperties,
              'audience',
              identityField
            );
            // Return rule errors if present
            matchedErrors = matchedAudiences.filter((match) =>
              Object.values(RuleError).includes(match as RuleError)
            );
            if (matchedErrors.length) return matchedErrors[0] as RuleError;
            if (matchedAudiences.length) {
              for (const item of matchedAudiences) {
                this._loggerManager?.info?.(
                  'DataManager.matchRulesByField()',
                  MESSAGES.AUDIENCE_MATCH.replace('#', item?.[identityField])
                );
              }
            }
            audiencesMatched = Boolean(matchedAudiences.length);
          } else {
            audiencesMatched = true; // Empty non-permanent experience audiences list means no restrictions
            this._loggerManager?.info?.(
              'DataManager.matchRulesByField()',
              MESSAGES.NON_PERMANENT_AUDIENCE_NOT_RESTRICTED
            );
          }
        } else {
          audiencesMatched = true; // Empty experience audiences list means no restrictions
          this._loggerManager?.info?.(
            'DataManager.matchRulesByField()',
            MESSAGES.AUDIENCE_NOT_RESTRICTED
          );
        }
      }
      // Get attached segmentation audiences
      segments = this.getItemsByIds(
        experience.audiences,
        'segments'
      ) as Array<ConfigSegment>;
      if (segments.length) {
        // Validate custom segments against segmentations
        matchedSegments = this.filterMatchedCustomSegments(segments, visitorId);
        if (matchedSegments.length) {
          for (const item of matchedSegments) {
            this._loggerManager?.info?.(
              'DataManager.matchRulesByField()',
              MESSAGES.SEGMENTATION_MATCH.replace('#', item?.[identityField])
            );
          }
        }
        segmentsMatched = Boolean(matchedSegments.length);
      } else {
        segmentsMatched = true; // Empty experience segmentation list means no restrictions
        this._loggerManager?.info?.(
          'DataManager.matchRulesByField()',
          MESSAGES.SEGMENTATION_NOT_RESTRICTED
        );
      }
      // If there are some matched audiences
      if (audiencesMatched && segmentsMatched) {
        // And experience has variations
        if (experience?.variations && experience?.variations?.length) {
          this._loggerManager?.info?.(
            'DataManager.matchRulesByField()',
            MESSAGES.EXPERIENCE_RULES_MATCHED
          );
          return experience;
        } else {
          this._loggerManager?.debug?.(
            'DataManager.matchRulesByField()',
            MESSAGES.VARIATIONS_NOT_FOUND,
            this._mapper({
              visitorProperties: visitorProperties,
              audiences: audiences
            })
          );
        }
      } else {
        this._loggerManager?.debug?.(
          'DataManager.matchRulesByField()',
          MESSAGES.AUDIENCE_NOT_MATCH,
          this._mapper({
            visitorProperties: visitorProperties,
            audiences: audiences
          })
        );
      }
    } else {
      this._loggerManager?.debug?.(
        'DataManager.matchRulesByField()',
        MESSAGES.EXPERIENCE_NOT_FOUND,
        this._mapper({
          identity: identity,
          identityField: identityField
        })
      );
    }
    return null;
  }

  /**
   * Retrieve variation for visitor
   * @param {string} visitorId
   * @param {string} identity Value of the field which name is provided in identityField
   * @param {IdentityField=} identityField Defaults to 'key'
   * @param {BucketingAttributes} attributes
   * @param {Record<any, any>} attributes.locationProperties
   * @param {Record<any, any>} attributes.visitorProperties
   * @param {boolean=} attributes.updateVisitorProperties
   * @param {string=} attributes.forceVariationId
   * @param {boolean=} attributes.enableTracking Defaults to `true`
   * @param {boolean=} attributes.asyncStorage Defaults to `true`
   * @param {string=} attributes.environment
   * @return {BucketedVariation | RuleError}
   * @private
   */
  private _getBucketingByField(
    visitorId: string,
    identity: string,
    identityField: IdentityField = 'key',
    attributes: BucketingAttributes
  ): BucketedVariation | RuleError {
    const {
      visitorProperties,
      locationProperties,
      updateVisitorProperties,
      forceVariationId,
      enableTracking = true,
      environment = this._environment
    } = attributes;
    this._loggerManager?.trace?.(
      'DataManager._getBucketingByField()',
      this._mapper({
        visitorId: visitorId,
        identity: identity,
        identityField: identityField,
        visitorProperties: visitorProperties,
        locationProperties: locationProperties,
        forceVariationId: forceVariationId,
        enableTracking: enableTracking,
        environment: environment
      })
    );

    // Retrieve the experience
    const experience = this.matchRulesByField(
      visitorId,
      identity,
      identityField,
      {visitorProperties, locationProperties, environment}
    );
    if (experience) {
      if (Object.values(RuleError).includes(experience as RuleError)) {
        return experience as RuleError;
      }
      return this._retrieveBucketing(
        visitorId,
        visitorProperties,
        updateVisitorProperties,
        experience as ConfigExperience,
        forceVariationId,
        enableTracking
      );
    }
    return null;
  }

  /**
   * Retrieve bucketing for Visitor
   * @param {string} visitorId
   * @param {Record<string, any> | null} visitorProperties
   * @param {boolean} updateVisitorProperties
   * @param {ConfigExperience} experience
   * @param {string=} forceVariationId
   * @param {boolean=} enableTracking Defaults to `true`
   * @return {BucketedVariation}
   * @private
   */
  private _retrieveBucketing(
    visitorId: string,
    visitorProperties: Record<string, any> | null,
    updateVisitorProperties: boolean,
    experience: ConfigExperience,
    forceVariationId?: string,
    enableTracking: boolean = true
  ): BucketedVariation {
    if (!visitorId || !experience) return null;
    if (!experience?.id) return null;
    let variation = null;
    let bucketedVariation = null;
    const storeKey = this.getStoreKey(visitorId);
    // Check that visitor id already bucketed and stored and skip bucketing logic
    const {bucketing, segments} = this.getData(visitorId) || {};
    const {[experience.id.toString()]: variationId} = bucketing || {};
    if (
      variationId &&
      (variation = this.retrieveVariation(experience.id, String(variationId)))
    ) {
      // If it's found log debug info. The return value will be formed next step
      this._loggerManager?.info?.(
        'DataManager._retrieveBucketing()',
        MESSAGES.BUCKETED_VISITOR_FOUND.replace('#', `#${variationId}`)
      );
      this._loggerManager?.debug?.(
        'DataManager._retrieveBucketing()',
        this._mapper({
          storeKey: storeKey,
          visitorId: visitorId,
          variationId: variationId
        })
      );
    } else {
      let variationId;
      if (
        forceVariationId &&
        (variation = this.retrieveVariation(
          experience.id,
          String(forceVariationId)
        ))
      ) {
        variationId = forceVariationId;
        // If it's found log debug info. The return value will be formed next step
        this._loggerManager?.info?.(
          'DataManager._retrieveBucketing()',
          MESSAGES.BUCKETED_VISITOR_FORCED.replace('#', `#${forceVariationId}`)
        );
        this._loggerManager?.debug?.(
          'DataManager._retrieveBucketing()',
          this._mapper({
            storeKey: storeKey,
            visitorId: visitorId,
            variationId: forceVariationId
          })
        );
      } else {
        // Build buckets where key is variation id and value is traffic distribution
        const buckets = experience.variations.reduce((bucket, variation) => {
          if (variation?.id)
            bucket[variation.id] = variation?.traffic_allocation || 100.0;
          return bucket;
        }, {}) as Record<string, number>;
        // Select bucket based for provided visitor id
        variationId = this._bucketingManager.getBucketForVisitor(
          buckets,
          visitorId,
          this._config?.bucketing?.excludeExperienceIdHash
            ? null
            : {experienceId: experience.id.toString()}
        );
      }
      if (variationId) {
        this._loggerManager?.info?.(
          'DataManager._retrieveBucketing()',
          MESSAGES.BUCKETED_VISITOR.replace('#', `#${variationId}`)
        );
        // Store the data
        if (updateVisitorProperties) {
          this.putData(visitorId, {
            bucketing: {
              [experience.id.toString()]: variationId
            },
            ...(visitorProperties ? {segments: visitorProperties} : {})
          });
        } else {
          this.putData(visitorId, {
            bucketing: {[experience.id.toString()]: variationId}
          });
        }
        if (enableTracking) {
          // Enqueue bucketing event to api
          const bucketingEvent: BucketingEvent = {
            experienceId: experience.id.toString(),
            variationId: variationId.toString()
          };
          const visitorEvent: VisitorTrackingEvents = {
            eventType: VisitorTrackingEvents.eventType.BUCKETING,
            data: bucketingEvent
          };
          this._apiManager.enqueue(visitorId, visitorEvent, segments);
          this._loggerManager?.trace?.(
            'DataManager._retrieveBucketing()',
            this._mapper({
              visitorEvent
            })
          );
        }
        // Retrieve and return variation
        variation = this.retrieveVariation(experience.id, String(variationId));
      } else {
        this._loggerManager?.error?.(
          'DataManager._retrieveBucketing()',
          ERROR_MESSAGES.UNABLE_TO_SELECT_BUCKET_FOR_VISITOR,
          this._mapper({
            visitorId: visitorId,
            experience: experience
          })
        );
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
   * @param {string} experienceId
   * @param {string} variationId
   * @return {ExperienceVariationConfig}
   * @private
   */
  private retrieveVariation(
    experienceId: string,
    variationId: string
  ): ExperienceVariationConfig {
    return this.getSubItem(
      'experiences',
      experienceId,
      'variations',
      variationId,
      'id',
      'id'
    ) as ExperienceVariationConfig;
  }

  reset() {
    this._bucketedVisitors = new Map();
  }

  /**
   * @param {string} visitorId
   * @param {StoreData} newData
   * @private
   */
  putData(visitorId: string, newData: StoreData = {}) {
    const storeKey = this.getStoreKey(visitorId);
    const storeData = this.getData(visitorId) || {};
    const isChanged = !objectDeepEqual(storeData, newData);
    if (isChanged) {
      const updatedData = objectDeepMerge(storeData, newData);
      this._bucketedVisitors.set(storeKey, updatedData);
      if (this._bucketedVisitors.size > this._localStoreLimit) {
        // Delete one of the oldest record
        for (const [key] of this._bucketedVisitors) {
          this._bucketedVisitors.delete(key);
          break;
        }
      }
      if (this.dataStoreManager) {
        const {segments: storedSegments = {}, ...data} = storeData;
        const {segments: reportSegments = {}} =
          this.filterReportSegments(storedSegments);
        const {segments: newSegments} = this.filterReportSegments(
          newData?.segments || {}
        );
        if (newSegments) {
          if (this._asyncStorage) {
            // Enqueue to store in dataStore
            this.dataStoreManager.enqueue(
              storeKey,
              objectDeepMerge(data, {
                segments: {...reportSegments, ...newSegments}
              })
            );
          } else {
            // Save now to store in dataStore
            this.dataStoreManager.set(
              storeKey,
              objectDeepMerge(data, {
                segments: {...reportSegments, ...newSegments}
              })
            );
          }
        } else {
          if (this._asyncStorage) {
            // Enqueue to store in dataStore
            this.dataStoreManager.enqueue(storeKey, updatedData);
          } else {
            // Save now to store in dataStore
            this.dataStoreManager.set(storeKey, updatedData);
          }
        }
      }
    }
  }

  /**
   * @param {string} visitorId
   * @return {StoreData} variation id
   * @private
   */
  getData(visitorId: string): StoreData {
    const storeKey = this.getStoreKey(visitorId);
    const memoryData = this._bucketedVisitors.get(storeKey) || null;
    if (this.dataStoreManager) {
      return objectDeepMerge(
        memoryData || {},
        this.dataStoreManager.get(storeKey) || {}
      );
    }
    return memoryData;
  }

  /**
   * @param {string} visitorId
   * @return {string} storeKey
   * @private
   */
  getStoreKey(visitorId: string): string {
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
    this._loggerManager?.trace?.(
      'DataManager.selectLocations()',
      this._mapper({
        items: items,
        locationProperties: locationProperties
      })
    );
    // Get locations from DataStore
    const {locations = []} = this.getData(visitorId) || {};
    const matchedRecords = [];
    let match;
    if (arrayNotEmpty(items)) {
      for (let i = 0, length = items.length; i < length; i++) {
        if (!items?.[i]?.rules) continue;
        match = this._ruleManager.isRuleMatched(
          locationProperties,
          items[i].rules,
          `ConfigLocation #${items[i][identityField]}`
        );
        const identity = items?.[i]?.[identityField]?.toString?.();
        if (match === true) {
          this._loggerManager?.info?.(
            'DataManager.selectLocations()',
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
              'DataManager.selectLocations()',
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
            'DataManager.selectLocations()',
            MESSAGES.LOCATION_DEACTIVATED.replace('#', `#${identity}`)
          );
        }
      }
    }
    // Store the data
    this.putData(visitorId, {
      locations
    });
    this._loggerManager?.debug?.(
      'DataManager.selectLocations()',
      this._mapper({
        matchedRecords: matchedRecords
      })
    );
    return matchedRecords;
  }

  /**
   * Retrieve variation for visitor
   * @param {string} visitorId
   * @param {string} key
   * @param {BucketingAttributes} attributes
   * @param {Record<any, any>} attributes.locationProperties
   * @param {Record<any, any>} attributes.visitorProperties
   * @param {boolean=} attributes.updateVisitorProperties
   * @param {boolean=} attributes.enableTracking
   * @param {string=} attributes.environment
   * @return {BucketedVariation | RuleError}
   */
  getBucketing(
    visitorId: string,
    key: string,
    attributes: BucketingAttributes
  ): BucketedVariation | RuleError {
    return this._getBucketingByField(visitorId, key, 'key', attributes);
  }

  /**
   * Retrieve variation for Visitor
   * @param {string} visitorId
   * @param {string} id
   * @param {BucketingAttributes} attributes
   * @param {Record<any, any>} attributes.locationProperties
   * @param {Record<any, any>} attributes.visitorProperties
   * @param {boolean=} attributes.updateVisitorProperties
   * @param {boolean=} attributes.enableTracking
   * @param {string=} attributes.environment
   * @return {BucketedVariation | RuleError}
   */
  getBucketingById(
    visitorId: string,
    id: string,
    attributes: BucketingAttributes
  ): BucketedVariation | RuleError {
    return this._getBucketingByField(visitorId, id, 'id', attributes);
  }

  /**
   * Process conversion event
   * @param {string} visitorId
   * @param {string} goalId
   * @param {Record<string, any>=} goalRule An object of key-value pairs that are used for goal matching
   * @param {Array<Record<GoalDataKey, number>>} goalData An array of object of key-value pairs
   * @param {VisitorSegments} segments
   */
  convert(
    visitorId: string,
    goalId: string,
    goalRule?: Record<string, any>,
    goalData?: Array<Record<GoalDataKey, number>>,
    segments?: VisitorSegments
  ): RuleError | boolean {
    const goal =
      typeof goalId === 'string'
        ? (this.getEntity(goalId, 'goals') as ConfigGoal)
        : (this.getEntityById(goalId, 'goals') as ConfigGoal);
    if (!goal?.id) {
      this._loggerManager?.error?.(
        'DataManager.convert()',
        MESSAGES.GOAL_NOT_FOUND
      );
      return;
    }

    if (goalRule) {
      if (!goal?.rules) return;
      const ruleMatched = this._ruleManager.isRuleMatched(
        goalRule,
        goal.rules,
        `ConfigGoal #${goalId}`
      );
      // Return rule errors if present
      if (Object.values(RuleError).includes(ruleMatched as RuleError))
        return ruleMatched as RuleError;
      if (!ruleMatched) {
        this._loggerManager?.error?.(
          'DataManager.convert()',
          MESSAGES.GOAL_RULE_NOT_MATCH
        );
        return;
      }
    }

    // Check that goal id already triggred and stored and skip tracking conversion event
    const storeKey = this.getStoreKey(visitorId);
    const {
      bucketing: bucketingData,
      goals: {[goalId.toString()]: goalTriggered} = {}
    } = this.getData(visitorId) || {};
    if (goalTriggered) {
      this._loggerManager?.debug?.(
        'DataManager.convert()',
        MESSAGES.GOAL_FOUND.replace('#', goalId.toString()),
        this._mapper({
          storeKey: storeKey,
          visitorId: visitorId,
          goalId: goalId
        })
      );
      return;
    } else {
      // Try to find a triggered goal in dataStore
      const {goals: {[goalId.toString()]: goalTriggered} = {}} =
        this.dataStoreManager?.get?.(storeKey) || {};
      if (goalTriggered) {
        this._loggerManager?.debug?.(
          'DataManager.convert()',
          MESSAGES.GOAL_FOUND.replace('#', goalId.toString()),
          this._mapper({
            storeKey: storeKey,
            visitorId: visitorId,
            goalId: goalId
          })
        );
        return;
      }
    }
    // Store the data
    this.putData(visitorId, {
      goals: {[goalId.toString()]: true}
    });

    const data: ConversionEvent = {
      goalId: goal.id
    };
    if (bucketingData) data.bucketingData = bucketingData;
    const event: VisitorTrackingEvents = {
      eventType: VisitorTrackingEvents.eventType.CONVERSION,
      data
    };
    this._apiManager.enqueue(visitorId, event, segments);
    // Split transaction events
    if (goalData) {
      const data: ConversionEvent = {
        goalId: goal.id,
        goalData: goalData as Array<Record<string, number>>
      };
      if (bucketingData) data.bucketingData = bucketingData;
      const event: VisitorTrackingEvents = {
        eventType: VisitorTrackingEvents.eventType.CONVERSION,
        data
      };
      this._apiManager.enqueue(visitorId, event, segments);
    }
    this._loggerManager?.trace?.(
      'DataManager.convert()',
      this._mapper({
        event
      })
    );

    return true;
  }

  /**
   * Get audiences that meet the visitorProperties
   * @param {Array<Record<any, any>>} items
   * @param {Record<string, any>} visitorProperties
   * @return {Array<Record<string, any> | RuleError>}
   */
  filterMatchedRecordsWithRule(
    items: Array<Record<string, any>>,
    visitorProperties: Record<string, any>,
    entityType: string,
    field: IdentityField = 'id'
  ): Array<Record<string, any> | RuleError> {
    this._loggerManager?.trace?.(
      'DataManager.filterMatchedRecordsWithRule()',
      this._mapper({
        items: items,
        visitorProperties: visitorProperties
      })
    );
    const matchedRecords = [];
    let match;
    if (arrayNotEmpty(items)) {
      for (let i = 0, length = items.length; i < length; i++) {
        if (!items?.[i]?.rules) continue;
        match = this._ruleManager.isRuleMatched(
          visitorProperties,
          items[i].rules,
          `${camelCase(entityType)} #${items[i][field]}`
        );
        if (match === true) {
          matchedRecords.push(items[i]);
        } else if (match !== false) {
          // catch rule errors
          matchedRecords.push(match);
        }
      }
    }
    this._loggerManager?.debug?.(
      'DataManager.filterMatchedRecordsWithRule()',
      this._mapper({
        matchedRecords: matchedRecords
      })
    );
    return matchedRecords;
  }

  /**
   * Get audiences that meet the custom segments
   * @param {Array<Record<any, any>>} items
   * @param {string} visitorId
   * @return {Array<Record<string, any>>}
   */
  filterMatchedCustomSegments(
    items: Array<Record<string, any>>,
    visitorId: string
  ): Array<Record<string, any>> {
    this._loggerManager?.trace?.(
      'DataManager.filterMatchedCustomSegments()',
      this._mapper({
        items: items,
        visitorId: visitorId
      })
    );
    // Get custom segments ID from DataStore
    const {
      segments: {[SegmentsKeys.CUSTOM_SEGMENTS]: customSegments = []} = {}
    } = this.getData(visitorId) || {};
    const matchedRecords = [];
    if (arrayNotEmpty(items)) {
      for (let i = 0, length = items.length; i < length; i++) {
        if (!items?.[i]?.id) continue;
        if (customSegments.includes(items[i].id)) {
          matchedRecords.push(items[i]);
        }
      }
    }
    this._loggerManager?.debug?.(
      'DataManager.filterMatchedCustomSegments()',
      this._mapper({
        matchedRecords: matchedRecords
      })
    );
    return matchedRecords;
  }

  /**
   * Extract report segments from other attribues in Visitor properties
   * @param {Record<string, any>=} visitorProperties An object of key-value pairs that are used for audience targeting
   * @return {Record<string, any>}
   */
  filterReportSegments(
    visitorProperties: Record<string, any>
  ): Record<string, any> {
    const segmentsKeys = Object.values(SegmentsKeys).map(
      (key) => key as string
    );
    const segments = {};
    const properties = {};
    for (const key in visitorProperties) {
      if (segmentsKeys.includes(key)) {
        segments[key] = visitorProperties[key];
      } else {
        properties[key] = visitorProperties[key];
      }
    }
    return {
      properties: Object.keys(properties).length ? properties : null,
      segments: Object.keys(segments).length ? segments : null
    };
  }

  /**
   * Get list of data entities
   * @param {string} entityType
   * @return {Array<Entity | string>}
   */
  getEntitiesList(entityType: string): Array<Entity | string> {
    let list = [];
    const mappedEntityType = DATA_ENTITIES_MAP[entityType] || entityType;
    if (this._dataEntities.indexOf(mappedEntityType) !== -1) {
      list = objectDeepValue(this._data, mappedEntityType) || [];
    }
    this._loggerManager?.trace?.(
      'DataManager.getEntitiesList()',
      this._mapper({
        entityType: mappedEntityType,
        list: list
      })
    );
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
    }, {}) as Record<string, Entity>;
  }

  /**
   *
   * @param {string} identity Value of the field which name is provided in identityField
   * @param {string} entityType
   * @param {IdentityField=} identityField Defaults to 'key'
   * @return {Entity}
   * @private
   */
  private _getEntityByField(
    identity: string,
    entityType: string,
    identityField: IdentityField = 'key'
  ): Entity {
    const mappedEntityType = DATA_ENTITIES_MAP[entityType] || entityType;
    this._loggerManager?.trace?.(
      'DataManager._getEntityByField()',
      this._mapper({
        identity: identity,
        entityType: mappedEntityType,
        identityField: identityField
      })
    );
    const list = this.getEntitiesList(mappedEntityType) as Array<Entity>;
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
   * @param {string} id
   * @param {string} entityType
   * @return {Entity}
   */
  getEntityById(id: string, entityType: string): Entity {
    return this._getEntityByField(id, entityType, 'id');
  }

  /**
   * Find the entity in list by ids
   * @param {Array<string>} ids
   * @param {string} entityType
   * @return {Array<Entity>}
   */
  getEntitiesByIds(ids: Array<string>, entityType: string): Array<Entity> {
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
   * @param {Array<string>} ids
   * @param {String} path
   * @return {Array<Record<string, any>>}
   */
  getItemsByIds(ids: Array<string>, path: string): Array<Record<string, any>> {
    this._loggerManager?.trace?.(
      'DataManager.getItemsByIds()',
      this._mapper({
        ids: ids,
        path: path
      })
    );
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
   * @return {Record<any, any>}
   */
  getSubItem(
    entityType: string,
    entityIdentity: string,
    subEntityType: string,
    subEntityIdentity: string,
    identityField: IdentityField,
    subIdentityField: IdentityField
  ): Record<any, any> {
    const entity = this._getEntityByField(
      entityIdentity,
      entityType,
      identityField
    );
    for (const subEntity of entity[subEntityType]) {
      if (subEntity[subIdentityField] === subEntityIdentity) {
        return subEntity;
      }
    }
    return null;
  }

  /**
   * Validates data object
   * @param data
   * @return {boolean}
   */
  isValidConfigData(data: ConfigResponseData): boolean {
    return (
      objectNotEmpty(data) &&
      ((!!data?.account_id && !!data?.project?.id) || Boolean(data['error']))
    );
  }
}
