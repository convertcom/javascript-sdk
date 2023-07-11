import { objectDeepMerge, objectDeepValue, arrayNotEmpty, objectNotEmpty } from '@convertcom/js-sdk-utils';
import { SystemEvents, ERROR_MESSAGES, DATA_ENTITIES, MESSAGES, RuleError, EventType, SegmentsKeys } from '@convertcom/js-sdk-enums';

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
const DEFAULT_BATCH_SIZE = 1;
const DEFAULT_RELEASE_INTERVAL = 5000;
/**
 * Data Store wrapper
 * @category Modules
 * @constructor
 * @implements {DataStoreManagerInterface}
 */
class DataStoreManager {
    /**
     * @param {Config=} config
     * @param {Object=} dependencies
     * @param {Object=} dependencies.dataStore
     * @param {EventManagerInterface=} dependencies.eventManager
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    constructor(config, { dataStore, eventManager, loggerManager } = {}) {
        this.batchSize = DEFAULT_BATCH_SIZE;
        this.releaseInterval = DEFAULT_RELEASE_INTERVAL;
        this._loggerManager = loggerManager;
        this._eventManager = eventManager;
        // TODO: Make this be configurable by config
        this.batchSize =
            // Number(objectDeepValue(config, 'events.batch_size')).valueOf() ||
            DEFAULT_BATCH_SIZE;
        this.releaseInterval =
            // Number(objectDeepValue(config, 'events.release_interval')).valueOf() ||
            DEFAULT_RELEASE_INTERVAL;
        this.dataStore = dataStore;
        this._requestsQueue = {};
    }
    set(key, data) {
        var _a, _b, _c, _d;
        try {
            (_b = (_a = this.dataStore) === null || _a === void 0 ? void 0 : _a.set) === null || _b === void 0 ? void 0 : _b.call(_a, key, data);
        }
        catch (error) {
            (_d = (_c = this._loggerManager) === null || _c === void 0 ? void 0 : _c.error) === null || _d === void 0 ? void 0 : _d.call(_c, 'DataStoreManager.set()', {
                error: error.message
            });
        }
    }
    get(key) {
        var _a, _b, _c, _d;
        try {
            return (_b = (_a = this.dataStore) === null || _a === void 0 ? void 0 : _a.get) === null || _b === void 0 ? void 0 : _b.call(_a, key);
        }
        catch (error) {
            (_d = (_c = this._loggerManager) === null || _c === void 0 ? void 0 : _c.error) === null || _d === void 0 ? void 0 : _d.call(_c, 'DataStoreManager.get()', {
                error: error.message
            });
        }
        return null;
    }
    enqueue(key, data) {
        var _a, _b;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'DataStoreManager.enqueue()', {
            key: key,
            data: data
        });
        const addData = {};
        addData[key] = data;
        this._requestsQueue = objectDeepMerge(this._requestsQueue, addData);
        if (Object.keys(this._requestsQueue).length >= this.batchSize) {
            this.releaseQueue('size');
        }
        else {
            if (Object.keys(this._requestsQueue).length === 1) {
                this.startQueue();
            }
        }
    }
    releaseQueue(reason) {
        var _a, _b, _c, _d;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'DataStoreManager.releaseQueue()', {
            reason: reason || ''
        });
        this.stopQueue();
        for (const key in this._requestsQueue) {
            this.set(key, this._requestsQueue[key]);
        }
        (_d = (_c = this._eventManager) === null || _c === void 0 ? void 0 : _c.fire) === null || _d === void 0 ? void 0 : _d.call(_c, SystemEvents.DATA_STORE_QUEUE_RELEASED, {
            reason: reason || ''
        });
    }
    stopQueue() {
        clearTimeout(this._requestsQueueTimerID);
    }
    startQueue() {
        this._requestsQueueTimerID = setTimeout(() => {
            this.releaseQueue('timeout');
        }, this.releaseInterval);
    }
    /**
     * dataStore setter
     * @param {any=} dataStore
     */
    set dataStore(dataStore) {
        var _a, _b;
        if (dataStore) {
            if (this.isValidDataStore(dataStore)) {
                this._dataStore = dataStore;
            }
            else {
                (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.error) === null || _b === void 0 ? void 0 : _b.call(_a, ERROR_MESSAGES.DATA_STORE_NOT_VALID);
            }
        }
    }
    /**
     * dataStore getter
     */
    get dataStore() {
        return this._dataStore;
    }
    /**
     * Validates dataStore object
     * @param {any=} dataStore
     * @return {boolean}
     */
    isValidDataStore(dataStore) {
        return (typeof dataStore === 'object' &&
            typeof dataStore['get'] === 'function' &&
            typeof dataStore['set'] === 'function');
    }
}

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
const LOCAL_STORE_LIMIT = 10000;
/**
 * Provides logic for data. Stores bucket with help of dataStore if it's provided
 * @category Modules
 * @constructor
 * @implements {DataManagerInterface}
 */
class DataManager {
    /**
     * @param {Config} config
     * @param {Object} dependencies
     * @param {ApiManagerInterface} dependencies.apiManager
     * @param {BucketingManagerInterface} dependencies.bucketingManager
     * @param {RuleManagerInterface} dependencies.ruleManager
     * @param {LogManagerInterface} dependencies.loggerManager
     */
    constructor(config, { bucketingManager, ruleManager, eventManager, apiManager, loggerManager }) {
        var _a, _b, _c, _d, _e;
        this._dataEntities = DATA_ENTITIES;
        this._localStoreLimit = LOCAL_STORE_LIMIT;
        this._bucketedVisitors = new Map();
        this._environment = config === null || config === void 0 ? void 0 : config.environment;
        this._apiManager = apiManager;
        this._bucketingManager = bucketingManager;
        this._ruleManager = ruleManager;
        this._loggerManager = loggerManager;
        this._eventManager = eventManager;
        this._config = config;
        this._data = objectDeepValue(config, 'data');
        this._accountId = (_a = this._data) === null || _a === void 0 ? void 0 : _a.account_id;
        this._projectId = (_c = (_b = this._data) === null || _b === void 0 ? void 0 : _b.project) === null || _c === void 0 ? void 0 : _c.id;
        this.dataStoreManager = objectDeepValue(config, 'dataStore');
        (_e = (_d = this._loggerManager) === null || _d === void 0 ? void 0 : _d.trace) === null || _e === void 0 ? void 0 : _e.call(_d, MESSAGES.DATA_CONSTRUCTOR, this);
    }
    set data(data) {
        var _a, _b, _c;
        if (this.isValidConfigData(data)) {
            this._data = data;
            this._accountId = data === null || data === void 0 ? void 0 : data.account_id;
            this._projectId = (_a = data === null || data === void 0 ? void 0 : data.project) === null || _a === void 0 ? void 0 : _a.id;
        }
        else {
            (_c = (_b = this._loggerManager) === null || _b === void 0 ? void 0 : _b.error) === null || _c === void 0 ? void 0 : _c.call(_b, ERROR_MESSAGES.CONFIG_DATA_NOT_VALID);
        }
    }
    /**
     * data getter
     */
    get data() {
        return this._data;
    }
    /**
     * dataStoreManager setter
     * @param {any=} dataStore
     */
    set dataStoreManager(dataStore) {
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
    get dataStoreManager() {
        return this._dataStoreManager;
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
    _getBucketingByField(visitorId, identity, visitorProperties, locationProperties, identityField = 'key', environment = this._environment) {
        var _a, _b, _c, _d, _e, _f, _g, _h, _j;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'DataManager._getBucketingByField()', {
            visitorId: visitorId,
            identity: identity,
            visitorProperties: visitorProperties,
            locationProperties: locationProperties,
            identityField: identityField,
            environment: environment
        });
        // Retrieve the experience
        const experience = this._getEntityByField(identity, 'experiences', identityField);
        // Retrieve archived experiences
        const archivedExperiences = this.getEntitiesList('archived_experiences');
        // Check whether the experience is archived
        const isArchivedExperience = !!archivedExperiences.find((id) => (experience === null || experience === void 0 ? void 0 : experience.id) == id);
        // Check environment
        const isEnvironmentMatch = Array.isArray(experience === null || experience === void 0 ? void 0 : experience.environments)
            ? !experience.environments.length || // skip if empty
                experience.environments.includes(environment)
            : true; // skip if no environments
        let matchedErrors = [];
        if (experience && !isArchivedExperience && isEnvironmentMatch) {
            let locationMatched = false;
            if (locationProperties) {
                if (Array.isArray(experience === null || experience === void 0 ? void 0 : experience.locations) &&
                    experience.locations.length) {
                    let matchedLocations = [];
                    // Get attached locations
                    const locations = this.getItemsByIds(experience.locations, 'locations');
                    if (locations.length) {
                        // Validate locationProperties against locations rules
                        matchedLocations = this.filterMatchedRecordsWithRule(locations, locationProperties);
                        // Return rule errors if present
                        matchedErrors = matchedLocations.filter((match) => Object.values(RuleError).includes(match));
                        if (matchedErrors.length)
                            return matchedErrors[0];
                    }
                    // If there are some matched locations
                    locationMatched = Boolean(matchedLocations.length);
                }
                else if (experience === null || experience === void 0 ? void 0 : experience.site_area) {
                    locationMatched = this._ruleManager.isRuleMatched(locationProperties, experience.site_area);
                    // Return rule errors if present
                    if (Object.values(RuleError).includes(locationMatched))
                        return locationMatched;
                }
                else {
                    // Empty experience locations list or unset Site Area means there's no restriction for the location
                    locationMatched = true;
                }
            }
            // Validate locationProperties against site area rules
            if (!locationProperties || locationMatched) {
                let audiences = [], segmentations = [], matchedAudiences = [], matchedSegmentations = [];
                if (visitorProperties) {
                    if (Array.isArray(experience === null || experience === void 0 ? void 0 : experience.audiences) &&
                        experience.audiences.length) {
                        // Get attached transient and/or permnent audiences
                        audiences = this.getItemsByIds(experience.audiences, 'audiences');
                        if (audiences.length) {
                            // Validate visitorProperties against audiences rules
                            matchedAudiences = this.filterMatchedRecordsWithRule(audiences, visitorProperties);
                            // Return rule errors if present
                            matchedErrors = matchedAudiences.filter((match) => Object.values(RuleError).includes(match));
                            if (matchedErrors.length)
                                return matchedErrors[0];
                        }
                        // Get attached segmentation audiences
                        segmentations = this.getItemsByIds(experience.audiences, 'segments');
                        if (segmentations.length) {
                            // Validate custom segments against segmentations
                            matchedSegmentations = this.filterMatchedCustomSegments(segmentations, visitorId);
                        }
                    }
                }
                // If there are some matched audiences
                if (!visitorProperties ||
                    matchedAudiences.length ||
                    matchedSegmentations.length ||
                    !audiences.length // Empty audiences list means there's no restriction for the audience
                ) {
                    // And experience has variations
                    if ((experience === null || experience === void 0 ? void 0 : experience.variations) && ((_c = experience === null || experience === void 0 ? void 0 : experience.variations) === null || _c === void 0 ? void 0 : _c.length)) {
                        return this._retrieveBucketing(visitorId, experience);
                    }
                    else {
                        (_e = (_d = this._loggerManager) === null || _d === void 0 ? void 0 : _d.debug) === null || _e === void 0 ? void 0 : _e.call(_d, MESSAGES.VARIATIONS_NOT_FOUND, {
                            visitorProperties: visitorProperties,
                            audiences: audiences
                        });
                    }
                }
                else {
                    (_g = (_f = this._loggerManager) === null || _f === void 0 ? void 0 : _f.debug) === null || _g === void 0 ? void 0 : _g.call(_f, MESSAGES.RULES_NOT_MATCH, {
                        visitorProperties: visitorProperties,
                        audiences: audiences
                    });
                }
            }
            else {
                (_j = (_h = this._loggerManager) === null || _h === void 0 ? void 0 : _h.debug) === null || _j === void 0 ? void 0 : _j.call(_h, MESSAGES.LOCATION_NOT_MATCH, {
                    locationProperties: locationProperties,
                    [(experience === null || experience === void 0 ? void 0 : experience.locations)
                        ? 'experiences[].variations[].locations'
                        : 'experiences[].variations[].site_area']: (experience === null || experience === void 0 ? void 0 : experience.locations) || (experience === null || experience === void 0 ? void 0 : experience.site_area) || ''
                });
            }
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
    _retrieveBucketing(visitorId, experience) {
        var _a, _b, _c, _d, _e, _f, _g, _h, _j, _k;
        if (!visitorId || !experience)
            return null;
        if (!(experience === null || experience === void 0 ? void 0 : experience.id))
            return null;
        let variation = null;
        let bucketedVariation = null;
        const storeKey = this.getStoreKey(visitorId);
        // Check that visitor id already bucketed and stored and skip bucketing logic
        const { bucketing: { [experience.id.toString()]: variationId } = {}, segments } = this.getLocalStore(visitorId) || {};
        if (variationId &&
            (variation = this.retrieveVariation(experience.id, variationId))) {
            // If it's found log debug info. The return value will be formed next step
            (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.debug) === null || _b === void 0 ? void 0 : _b.call(_a, MESSAGES.BUCKETED_VISITOR_FOUND, {
                storeKey: storeKey,
                visitorId: visitorId,
                variationId: variationId
            });
        }
        else {
            // Try to find a bucketed visitor in dataStore
            let { bucketing: { [experience.id.toString()]: variationId } = {} } = ((_d = (_c = this.dataStoreManager) === null || _c === void 0 ? void 0 : _c.get) === null || _d === void 0 ? void 0 : _d.call(_c, storeKey)) || {};
            if (variationId &&
                (variation = this.retrieveVariation(experience.id, variationId))) {
                // Store the data in local variable
                this.putLocalStore(visitorId, Object.assign({ bucketing: { [experience.id.toString()]: variationId } }, (segments ? { segments } : {})));
                // If it's found log debug info. The return value will be formed next step
                (_f = (_e = this._loggerManager) === null || _e === void 0 ? void 0 : _e.debug) === null || _f === void 0 ? void 0 : _f.call(_e, MESSAGES.BUCKETED_VISITOR_FOUND, {
                    storeKey: storeKey,
                    visitorId: visitorId,
                    variationId: variationId
                });
            }
            else {
                // Build buckets where key is variation id and value is traffic distribution
                const buckets = experience.variations.reduce((bucket, variation) => {
                    if (variation === null || variation === void 0 ? void 0 : variation.id)
                        bucket[variation.id] = (variation === null || variation === void 0 ? void 0 : variation.traffic_allocation) || 100.0;
                    return bucket;
                }, {});
                // Select bucket based for provided visitor id
                variationId = this._bucketingManager.getBucketForVisitor(buckets, visitorId);
                if (variationId) {
                    // Store the data in local variable
                    const storeData = Object.assign({ bucketing: { [experience.id.toString()]: variationId } }, (segments ? { segments } : {}));
                    this.putLocalStore(visitorId, storeData);
                    // Enqueue to store in dataStore
                    this.dataStoreManager.enqueue(storeKey, storeData);
                    // Enqueue bucketing event to api
                    const bucketingEvent = {
                        experienceId: experience.id,
                        variationId
                    };
                    const visitorEvent = {
                        eventType: EventType.BUCKETING,
                        data: bucketingEvent
                    };
                    this._apiManager.enqueue(visitorId, visitorEvent, segments);
                    (_h = (_g = this._loggerManager) === null || _g === void 0 ? void 0 : _g.trace) === null || _h === void 0 ? void 0 : _h.call(_g, 'DataManager._retrieveBucketing()', {
                        visitorEvent
                    });
                    // Retrieve and return variation
                    variation = this.retrieveVariation(experience.id, variationId);
                }
                else {
                    (_k = (_j = this._loggerManager) === null || _j === void 0 ? void 0 : _j.error) === null || _k === void 0 ? void 0 : _k.call(_j, ERROR_MESSAGES.UNABLE_TO_SELECT_BUCKET_FOR_VISITOR, {
                        visitorId: visitorId,
                        experience: experience
                    });
                }
            }
        }
        // Build the response as bucketed variation object
        if (variation) {
            bucketedVariation = Object.assign({
                experienceId: experience === null || experience === void 0 ? void 0 : experience.id,
                experienceName: experience === null || experience === void 0 ? void 0 : experience.name,
                experienceKey: experience === null || experience === void 0 ? void 0 : experience.key
            }, variation);
        }
        return bucketedVariation;
    }
    /**
     * @param {Id} experienceId
     * @param {Id} variationId
     * @return {Variation}
     * @private
     */
    retrieveVariation(experienceId, variationId) {
        return this.getSubItem('experiences', experienceId, 'variations', variationId, 'id', 'id');
    }
    /**
     * @param {Id} visitorId
     * @param {StoreData} storeData
     * @private
     */
    putLocalStore(visitorId, storeData) {
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
    getLocalStore(visitorId) {
        const storeKey = this.getStoreKey(visitorId);
        return this._bucketedVisitors.get(storeKey) || null;
    }
    /**
     * @param {Id} visitorId
     * @return {string} storeKey
     * @private
     */
    getStoreKey(visitorId) {
        return `${this._accountId}-${this._projectId}-${visitorId}`;
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
    getBucketing(visitorId, key, visitorProperties, locationProperties, environment = this._environment) {
        return this._getBucketingByField(visitorId, key, visitorProperties, locationProperties, 'key', environment);
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
    getBucketingById(visitorId, id, visitorProperties, locationProperties, environment = this._environment) {
        return this._getBucketingByField(visitorId, id, visitorProperties, locationProperties, 'id', environment);
    }
    /**
     * Process conversion event
     * @param {Id} visitorId
     * @param {Id} goalId
     * @param {Record<string, any>=} goalRule An object of key-value pairs that are used for goal matching
     * @param {Array<Record<GoalDataKey, number>>} goalData An array of object of key-value pairs
     * @param {SegmentsData} segments
     */
    convert(visitorId, goalId, goalRule, goalData, segments) {
        var _a, _b, _c, _d, _e, _f;
        const goal = typeof goalId === 'string'
            ? this.getEntity(goalId, 'goals')
            : this.getEntityById(goalId, 'goals');
        if (!(goal === null || goal === void 0 ? void 0 : goal.id)) {
            (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.error) === null || _b === void 0 ? void 0 : _b.call(_a, MESSAGES.GOAL_NOT_FOUND);
            return;
        }
        if (goalRule) {
            if (!(goal === null || goal === void 0 ? void 0 : goal.rules))
                return;
            const ruleMatched = this._ruleManager.isRuleMatched(goalRule, goal.rules);
            // Return rule errors if present
            if (Object.values(RuleError).includes(ruleMatched))
                return ruleMatched;
            if (!ruleMatched) {
                (_d = (_c = this._loggerManager) === null || _c === void 0 ? void 0 : _c.error) === null || _d === void 0 ? void 0 : _d.call(_c, MESSAGES.GOAL_RULE_NOT_MATCH);
                return;
            }
        }
        const data = {
            goalId: goal.id
        };
        const { bucketing: bucketingData } = this.getLocalStore(visitorId) || {};
        if (bucketingData)
            data.bucketingData = bucketingData;
        const event = {
            eventType: EventType.CONVERSION,
            data
        };
        this._apiManager.enqueue(visitorId, event, segments);
        // Split transaction events
        if (goalData) {
            const data = {
                goalId: goal.id,
                goalData
            };
            if (bucketingData)
                data.bucketingData = bucketingData;
            const event = {
                eventType: EventType.CONVERSION,
                data
            };
            this._apiManager.enqueue(visitorId, event, segments);
        }
        (_f = (_e = this._loggerManager) === null || _e === void 0 ? void 0 : _e.trace) === null || _f === void 0 ? void 0 : _f.call(_e, 'DataManager.convert()', {
            event
        });
    }
    /**
     * Get audiences that meet the visitorProperties
     * @param {Array<Record<any, any>>} items
     * @param {Record<string, any>} visitorProperties
     * @return {Array<Record<string, any> | RuleError>}
     */
    filterMatchedRecordsWithRule(items, visitorProperties) {
        var _a, _b, _c, _d, _e;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'DataManager.filterMatchedRecordsWithRule()', {
            items: items,
            visitorProperties: visitorProperties
        });
        const matchedRecords = [];
        let match;
        if (arrayNotEmpty(items)) {
            for (let i = 0, length = items.length; i < length; i++) {
                if (!((_c = items === null || items === void 0 ? void 0 : items[i]) === null || _c === void 0 ? void 0 : _c.rules))
                    continue;
                match = this._ruleManager.isRuleMatched(visitorProperties, items[i].rules);
                if (match === true) {
                    matchedRecords.push(items[i]);
                }
                else if (match !== false) {
                    // catch rule errors
                    matchedRecords.push(match);
                }
            }
        }
        (_e = (_d = this._loggerManager) === null || _d === void 0 ? void 0 : _d.debug) === null || _e === void 0 ? void 0 : _e.call(_d, 'DataManager.filterMatchedRecordsWithRule()', {
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
    filterMatchedCustomSegments(items, visitorId) {
        var _a, _b, _c, _d, _e;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'DataManager.filterMatchedCustomSegments()', {
            items: items,
            visitorId: visitorId
        });
        // Check that custom segments are matched
        const storeData = this.getLocalStore(visitorId) || {};
        // Get custom segments ID from DataStore
        const { segments: { [SegmentsKeys.CUSTOM_SEGMENTS]: customSegments = [] } = {} } = storeData;
        const matchedRecords = [];
        if (arrayNotEmpty(items)) {
            for (let i = 0, length = items.length; i < length; i++) {
                if (!((_c = items === null || items === void 0 ? void 0 : items[i]) === null || _c === void 0 ? void 0 : _c.id))
                    continue;
                if (customSegments.includes(items[i].id)) {
                    matchedRecords.push(items[i]);
                }
            }
        }
        (_e = (_d = this._loggerManager) === null || _d === void 0 ? void 0 : _d.debug) === null || _e === void 0 ? void 0 : _e.call(_d, 'DataManager.filterMatchedCustomSegments()', {
            matchedRecords: matchedRecords
        });
        return matchedRecords;
    }
    /**
     * Get list of data entities
     * @param {string} entityType
     * @return {Array<Entity | Id>}
     */
    getEntitiesList(entityType) {
        var _a, _b;
        let list = [];
        if (this._dataEntities.indexOf(entityType) !== -1) {
            list = objectDeepValue(this._data, entityType) || [];
        }
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'DataManager.getEntitiesList()', {
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
    getEntitiesListObject(entityType, field = 'id') {
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
    _getEntityByField(identity, entityType, identityField = 'key') {
        var _a, _b, _c;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'DataManager._getEntityByField()', {
            identity: identity,
            entityType: entityType,
            identityField: identityField
        });
        const list = this.getEntitiesList(entityType);
        if (arrayNotEmpty(list)) {
            for (let i = 0, length = list.length; i < length; i++) {
                if (list[i] && ((_c = list[i]) === null || _c === void 0 ? void 0 : _c[identityField]) === identity) {
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
    getEntity(key, entityType) {
        return this._getEntityByField(key, entityType, 'key');
    }
    /**
     * Find the entity in list by keys
     * @param {Array<string>} keys
     * @param {string} entityType
     * @return {Array<Entity>}
     */
    getEntities(keys, entityType) {
        return this.getItemsByKeys(keys, entityType);
    }
    /**
     * Find the entity in list by id
     * @param {Id} id
     * @param {string} entityType
     * @return {Entity}
     */
    getEntityById(id, entityType) {
        return this._getEntityByField(id, entityType, 'id');
    }
    /**
     * Find the entity in list by ids
     * @param {Array<Id>} ids
     * @param {string} entityType
     * @return {Array<Entity>}
     */
    getEntitiesByIds(ids, entityType) {
        return this.getItemsByIds(ids, entityType);
    }
    /**
     * Find the items in list by  keys
     * @param {Array<string>} keys
     * @param {string} path
     * @return {Array<Record<string, any>>}
     */
    getItemsByKeys(keys, path) {
        var _a;
        const list = this.getEntitiesList(path);
        const items = [];
        if (arrayNotEmpty(list)) {
            for (let i = 0, length = list.length; i < length; i++) {
                if (keys.indexOf((_a = list[i]) === null || _a === void 0 ? void 0 : _a.key) !== -1) {
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
    getItemsByIds(ids, path) {
        var _a, _b, _c;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'DataManager.getItemsByIds()', {
            ids: ids,
            path: path
        });
        const items = [];
        if (arrayNotEmpty(ids)) {
            const list = this.getEntitiesList(path);
            if (arrayNotEmpty(list)) {
                for (let i = 0, length = list.length; i < length; i++) {
                    if (ids.indexOf((_c = list[i]) === null || _c === void 0 ? void 0 : _c.id) !== -1) {
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
    getSubItem(entityType, entityIdentity, subEntityType, subEntityIdentity, identityField, subIdentityField) {
        var _a;
        const entity = this._getEntityByField(entityIdentity, entityType, identityField);
        for (const k in entity[subEntityType]) {
            if (((_a = entity[subEntityType][k]) === null || _a === void 0 ? void 0 : _a[subIdentityField]) == subEntityIdentity) {
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
    isValidConfigData(data) {
        var _a;
        return objectNotEmpty(data) && !!(data === null || data === void 0 ? void 0 : data.account_id) && !!((_a = data === null || data === void 0 ? void 0 : data.project) === null || _a === void 0 ? void 0 : _a.id);
    }
}

export { DataManager, DataStoreManager };
//# sourceMappingURL=index.mjs.map
