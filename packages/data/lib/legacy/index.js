'use strict';

var jsSdkUtils = require('@convertcom/js-sdk-utils');
var jsSdkEnums = require('@convertcom/js-sdk-enums');

/******************************************************************************
Copyright (c) Microsoft Corporation.

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
***************************************************************************** */
/* global Reflect, Promise, SuppressedError, Symbol */


var __assign = function() {
    __assign = Object.assign || function __assign(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p)) t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};

function __values(o) {
    var s = typeof Symbol === "function" && Symbol.iterator, m = s && o[s], i = 0;
    if (m) return m.call(o);
    if (o && typeof o.length === "number") return {
        next: function () {
            if (o && i >= o.length) o = void 0;
            return { value: o && o[i++], done: !o };
        }
    };
    throw new TypeError(s ? "Object is not iterable." : "Symbol.iterator is not defined.");
}

function __read(o, n) {
    var m = typeof Symbol === "function" && o[Symbol.iterator];
    if (!m) return o;
    var i = m.call(o), r, ar = [], e;
    try {
        while ((n === void 0 || n-- > 0) && !(r = i.next()).done) ar.push(r.value);
    }
    catch (error) { e = { error: error }; }
    finally {
        try {
            if (r && !r.done && (m = i["return"])) m.call(i);
        }
        finally { if (e) throw e.error; }
    }
    return ar;
}

typeof SuppressedError === "function" ? SuppressedError : function (error, suppressed, message) {
    var e = new Error(message);
    return e.name = "SuppressedError", e.error = error, e.suppressed = suppressed, e;
};

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
var DEFAULT_BATCH_SIZE = 1;
var DEFAULT_RELEASE_INTERVAL = 5000;
/**
 * Data Store wrapper
 * @category Modules
 * @constructor
 * @implements {DataStoreManagerInterface}
 */
var DataStoreManager = /** @class */ (function () {
    /**
     * @param {Config=} config
     * @param {Object=} dependencies
     * @param {Object=} dependencies.dataStore
     * @param {EventManagerInterface=} dependencies.eventManager
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    function DataStoreManager(config, _a) {
        var _b = _a === void 0 ? {} : _a, dataStore = _b.dataStore, eventManager = _b.eventManager, loggerManager = _b.loggerManager;
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
    DataStoreManager.prototype.set = function (key, data) {
        var _a, _b, _c, _d;
        try {
            (_b = (_a = this.dataStore) === null || _a === void 0 ? void 0 : _a.set) === null || _b === void 0 ? void 0 : _b.call(_a, key, data);
        }
        catch (error) {
            (_d = (_c = this._loggerManager) === null || _c === void 0 ? void 0 : _c.error) === null || _d === void 0 ? void 0 : _d.call(_c, 'DataStoreManager.set()', {
                error: error.message
            });
        }
    };
    DataStoreManager.prototype.get = function (key) {
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
    };
    DataStoreManager.prototype.enqueue = function (key, data) {
        var _a, _b;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'DataStoreManager.enqueue()', {
            key: key,
            data: data
        });
        var addData = {};
        addData[key] = data;
        this._requestsQueue = jsSdkUtils.objectDeepMerge(this._requestsQueue, addData);
        if (Object.keys(this._requestsQueue).length >= this.batchSize) {
            this.releaseQueue('size');
        }
        else {
            if (Object.keys(this._requestsQueue).length === 1) {
                this.startQueue();
            }
        }
    };
    DataStoreManager.prototype.releaseQueue = function (reason) {
        var _a, _b, _c, _d;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'DataStoreManager.releaseQueue()', {
            reason: reason || ''
        });
        this.stopQueue();
        for (var key in this._requestsQueue) {
            this.set(key, this._requestsQueue[key]);
        }
        (_d = (_c = this._eventManager) === null || _c === void 0 ? void 0 : _c.fire) === null || _d === void 0 ? void 0 : _d.call(_c, jsSdkEnums.SystemEvents.DATA_STORE_QUEUE_RELEASED, {
            reason: reason || ''
        });
    };
    DataStoreManager.prototype.stopQueue = function () {
        clearTimeout(this._requestsQueueTimerID);
    };
    DataStoreManager.prototype.startQueue = function () {
        var _this = this;
        this._requestsQueueTimerID = setTimeout(function () {
            _this.releaseQueue('timeout');
        }, this.releaseInterval);
    };
    Object.defineProperty(DataStoreManager.prototype, "dataStore", {
        /**
         * dataStore getter
         */
        get: function () {
            return this._dataStore;
        },
        /**
         * dataStore setter
         * @param {any=} dataStore
         */
        set: function (dataStore) {
            var _a, _b;
            if (dataStore) {
                if (this.isValidDataStore(dataStore)) {
                    this._dataStore = dataStore;
                }
                else {
                    (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.error) === null || _b === void 0 ? void 0 : _b.call(_a, jsSdkEnums.ERROR_MESSAGES.DATA_STORE_NOT_VALID);
                }
            }
        },
        enumerable: false,
        configurable: true
    });
    /**
     * Validates dataStore object
     * @param {any=} dataStore
     * @return {boolean}
     */
    DataStoreManager.prototype.isValidDataStore = function (dataStore) {
        return (typeof dataStore === 'object' &&
            typeof dataStore['get'] === 'function' &&
            typeof dataStore['set'] === 'function');
    };
    return DataStoreManager;
}());

var LOCAL_STORE_LIMIT = 10000;
/**
 * Provides logic for data. Stores bucket with help of dataStore if it's provided
 * @category Modules
 * @constructor
 * @implements {DataManagerInterface}
 */
var DataManager = /** @class */ (function () {
    /**
     * @param {Config} config
     * @param {Object} dependencies
     * @param {ApiManagerInterface} dependencies.apiManager
     * @param {BucketingManagerInterface} dependencies.bucketingManager
     * @param {RuleManagerInterface} dependencies.ruleManager
     * @param {LogManagerInterface} dependencies.loggerManager
     */
    function DataManager(config, _a) {
        var bucketingManager = _a.bucketingManager, ruleManager = _a.ruleManager, eventManager = _a.eventManager, apiManager = _a.apiManager, loggerManager = _a.loggerManager;
        var _b, _c, _d, _e, _f;
        this._dataEntities = jsSdkEnums.DATA_ENTITIES;
        this._localStoreLimit = LOCAL_STORE_LIMIT;
        this._bucketedVisitors = new Map();
        this._environment = config === null || config === void 0 ? void 0 : config.environment;
        this._apiManager = apiManager;
        this._bucketingManager = bucketingManager;
        this._ruleManager = ruleManager;
        this._loggerManager = loggerManager;
        this._eventManager = eventManager;
        this._config = config;
        this._data = jsSdkUtils.objectDeepValue(config, 'data');
        this._accountId = (_b = this._data) === null || _b === void 0 ? void 0 : _b.account_id;
        this._projectId = (_d = (_c = this._data) === null || _c === void 0 ? void 0 : _c.project) === null || _d === void 0 ? void 0 : _d.id;
        this.dataStoreManager = jsSdkUtils.objectDeepValue(config, 'dataStore');
        (_f = (_e = this._loggerManager) === null || _e === void 0 ? void 0 : _e.trace) === null || _f === void 0 ? void 0 : _f.call(_e, jsSdkEnums.MESSAGES.DATA_CONSTRUCTOR, this);
    }
    Object.defineProperty(DataManager.prototype, "data", {
        /**
         * data getter
         */
        get: function () {
            return this._data;
        },
        set: function (data) {
            var _a, _b, _c;
            if (this.isValidConfigData(data)) {
                this._data = data;
                this._accountId = data === null || data === void 0 ? void 0 : data.account_id;
                this._projectId = (_a = data === null || data === void 0 ? void 0 : data.project) === null || _a === void 0 ? void 0 : _a.id;
            }
            else {
                (_c = (_b = this._loggerManager) === null || _b === void 0 ? void 0 : _b.error) === null || _c === void 0 ? void 0 : _c.call(_b, jsSdkEnums.ERROR_MESSAGES.CONFIG_DATA_NOT_VALID);
            }
        },
        enumerable: false,
        configurable: true
    });
    Object.defineProperty(DataManager.prototype, "dataStoreManager", {
        /**
         * dataStoreManager getter
         */
        get: function () {
            return this._dataStoreManager;
        },
        /**
         * dataStoreManager setter
         * @param {any=} dataStore
         */
        set: function (dataStore) {
            this._dataStoreManager = null;
            this._dataStoreManager = new DataStoreManager(this._config, {
                dataStore: dataStore,
                eventManager: this._eventManager,
                loggerManager: this._loggerManager
            });
        },
        enumerable: false,
        configurable: true
    });
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
    DataManager.prototype._getBucketingByField = function (visitorId, identity, visitorProperties, locationProperties, identityField, environment) {
        var _a;
        var _b, _c, _d, _e, _f, _g, _h, _j, _k;
        if (identityField === void 0) { identityField = 'key'; }
        if (environment === void 0) { environment = this._environment; }
        (_c = (_b = this._loggerManager) === null || _b === void 0 ? void 0 : _b.trace) === null || _c === void 0 ? void 0 : _c.call(_b, 'DataManager._getBucketingByField()', {
            visitorId: visitorId,
            identity: identity,
            visitorProperties: visitorProperties,
            locationProperties: locationProperties,
            identityField: identityField,
            environment: environment
        });
        // Retrieve the experience
        var experience = this._getEntityByField(identity, 'experiences', identityField);
        // Retrieve archived experiences
        var archivedExperiences = this.getEntitiesList('archived_experiences');
        // Check whether the experience is archived
        var isArchivedExperience = !!archivedExperiences.find(function (id) { return (experience === null || experience === void 0 ? void 0 : experience.id) == id; });
        // Check environment
        var isEnvironmentMatch = Array.isArray(experience === null || experience === void 0 ? void 0 : experience.environments)
            ? !experience.environments.length || // skip if empty
                experience.environments.includes(environment)
            : true; // skip if no environments
        var matchedErrors = [];
        if (experience && !isArchivedExperience && isEnvironmentMatch) {
            var locationMatched = false;
            if (locationProperties) {
                if (Array.isArray(experience === null || experience === void 0 ? void 0 : experience.locations) &&
                    experience.locations.length) {
                    var matchedLocations = [];
                    // Get attached locations
                    var locations = this.getItemsByIds(experience.locations, 'locations');
                    if (locations.length) {
                        // Validate locationProperties against locations rules
                        matchedLocations = this.filterMatchedRecordsWithRule(locations, locationProperties);
                        // Return rule errors if present
                        matchedErrors = matchedLocations.filter(function (match) {
                            return Object.values(jsSdkEnums.RuleError).includes(match);
                        });
                        if (matchedErrors.length)
                            return matchedErrors[0];
                    }
                    // If there are some matched locations
                    locationMatched = Boolean(matchedLocations.length);
                }
                else if (experience === null || experience === void 0 ? void 0 : experience.site_area) {
                    locationMatched = this._ruleManager.isRuleMatched(locationProperties, experience.site_area);
                    // Return rule errors if present
                    if (Object.values(jsSdkEnums.RuleError).includes(locationMatched))
                        return locationMatched;
                }
                else {
                    // Empty experience locations list or unset Site Area means there's no restriction for the location
                    locationMatched = true;
                }
            }
            // Validate locationProperties against site area rules
            if (!locationProperties || locationMatched) {
                var audiences = [], segmentations = [], matchedAudiences = [], matchedSegmentations = [];
                if (visitorProperties) {
                    if (Array.isArray(experience === null || experience === void 0 ? void 0 : experience.audiences) &&
                        experience.audiences.length) {
                        // Get attached transient and/or permnent audiences
                        audiences = this.getItemsByIds(experience.audiences, 'audiences');
                        if (audiences.length) {
                            // Validate visitorProperties against audiences rules
                            matchedAudiences = this.filterMatchedRecordsWithRule(audiences, visitorProperties);
                            // Return rule errors if present
                            matchedErrors = matchedAudiences.filter(function (match) {
                                return Object.values(jsSdkEnums.RuleError).includes(match);
                            });
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
                    if ((experience === null || experience === void 0 ? void 0 : experience.variations) && ((_d = experience === null || experience === void 0 ? void 0 : experience.variations) === null || _d === void 0 ? void 0 : _d.length)) {
                        return this._retrieveBucketing(visitorId, experience);
                    }
                    else {
                        (_f = (_e = this._loggerManager) === null || _e === void 0 ? void 0 : _e.debug) === null || _f === void 0 ? void 0 : _f.call(_e, jsSdkEnums.MESSAGES.VARIATIONS_NOT_FOUND, {
                            visitorProperties: visitorProperties,
                            audiences: audiences
                        });
                    }
                }
                else {
                    (_h = (_g = this._loggerManager) === null || _g === void 0 ? void 0 : _g.debug) === null || _h === void 0 ? void 0 : _h.call(_g, jsSdkEnums.MESSAGES.RULES_NOT_MATCH, {
                        visitorProperties: visitorProperties,
                        audiences: audiences
                    });
                }
            }
            else {
                (_k = (_j = this._loggerManager) === null || _j === void 0 ? void 0 : _j.debug) === null || _k === void 0 ? void 0 : _k.call(_j, jsSdkEnums.MESSAGES.LOCATION_NOT_MATCH, (_a = {
                        locationProperties: locationProperties
                    },
                    _a[(experience === null || experience === void 0 ? void 0 : experience.locations)
                        ? 'experiences[].variations[].locations'
                        : 'experiences[].variations[].site_area'] = (experience === null || experience === void 0 ? void 0 : experience.locations) || (experience === null || experience === void 0 ? void 0 : experience.site_area) || '',
                    _a));
            }
        }
        return null;
    };
    /**
     * Retrieve bucketing for Visitor
     * @param {Id} visitorId
     * @param {Experience} experience
     * @return {BucketedVariation}
     * @private
     */
    DataManager.prototype._retrieveBucketing = function (visitorId, experience) {
        var _a, _b;
        var _c, _d, _e, _f, _g, _h, _j, _k, _l, _m;
        if (!visitorId || !experience)
            return null;
        if (!(experience === null || experience === void 0 ? void 0 : experience.id))
            return null;
        var variation = null;
        var bucketedVariation = null;
        var storeKey = this.getStoreKey(visitorId);
        // Check that visitor id already bucketed and stored and skip bucketing logic
        var _o = this.getLocalStore(visitorId) || {}, _p = _o.bucketing, _q = _p === void 0 ? {} : _p, _r = experience.id.toString(), variationId = _q[_r], segments = _o.segments;
        if (variationId &&
            (variation = this.retrieveVariation(experience.id, variationId))) {
            // If it's found log debug info. The return value will be formed next step
            (_d = (_c = this._loggerManager) === null || _c === void 0 ? void 0 : _c.debug) === null || _d === void 0 ? void 0 : _d.call(_c, jsSdkEnums.MESSAGES.BUCKETED_VISITOR_FOUND, {
                storeKey: storeKey,
                visitorId: visitorId,
                variationId: variationId
            });
        }
        else {
            // Try to find a bucketed visitor in dataStore
            var _s = ((_f = (_e = this.dataStoreManager) === null || _e === void 0 ? void 0 : _e.get) === null || _f === void 0 ? void 0 : _f.call(_e, storeKey)) || {}, _t = _s.bucketing, _u = _t === void 0 ? {} : _t, _v = experience.id.toString(), variationId_1 = _u[_v];
            if (variationId_1 &&
                (variation = this.retrieveVariation(experience.id, variationId_1))) {
                // Store the data in local variable
                this.putLocalStore(visitorId, __assign({ bucketing: (_a = {}, _a[experience.id.toString()] = variationId_1, _a) }, (segments ? { segments: segments } : {})));
                // If it's found log debug info. The return value will be formed next step
                (_h = (_g = this._loggerManager) === null || _g === void 0 ? void 0 : _g.debug) === null || _h === void 0 ? void 0 : _h.call(_g, jsSdkEnums.MESSAGES.BUCKETED_VISITOR_FOUND, {
                    storeKey: storeKey,
                    visitorId: visitorId,
                    variationId: variationId_1
                });
            }
            else {
                // Build buckets where key is variation id and value is traffic distribution
                var buckets = experience.variations.reduce(function (bucket, variation) {
                    if (variation === null || variation === void 0 ? void 0 : variation.id)
                        bucket[variation.id] = (variation === null || variation === void 0 ? void 0 : variation.traffic_allocation) || 100.0;
                    return bucket;
                }, {});
                // Select bucket based for provided visitor id
                variationId_1 = this._bucketingManager.getBucketForVisitor(buckets, visitorId);
                if (variationId_1) {
                    // Store the data in local variable
                    var storeData = __assign({ bucketing: (_b = {}, _b[experience.id.toString()] = variationId_1, _b) }, (segments ? { segments: segments } : {}));
                    this.putLocalStore(visitorId, storeData);
                    // Enqueue to store in dataStore
                    this.dataStoreManager.enqueue(storeKey, storeData);
                    // Enqueue bucketing event to api
                    var bucketingEvent = {
                        experienceId: experience.id,
                        variationId: variationId_1
                    };
                    var visitorEvent = {
                        eventType: jsSdkEnums.EventType.BUCKETING,
                        data: bucketingEvent
                    };
                    this._apiManager.enqueue(visitorId, visitorEvent, segments);
                    (_k = (_j = this._loggerManager) === null || _j === void 0 ? void 0 : _j.trace) === null || _k === void 0 ? void 0 : _k.call(_j, 'DataManager._retrieveBucketing()', {
                        visitorEvent: visitorEvent
                    });
                    // Retrieve and return variation
                    variation = this.retrieveVariation(experience.id, variationId_1);
                }
                else {
                    (_m = (_l = this._loggerManager) === null || _l === void 0 ? void 0 : _l.error) === null || _m === void 0 ? void 0 : _m.call(_l, jsSdkEnums.ERROR_MESSAGES.UNABLE_TO_SELECT_BUCKET_FOR_VISITOR, {
                        visitorId: visitorId,
                        experience: experience
                    });
                }
            }
        }
        // Build the response as bucketed variation object
        if (variation) {
            bucketedVariation = __assign({
                experienceId: experience === null || experience === void 0 ? void 0 : experience.id,
                experienceName: experience === null || experience === void 0 ? void 0 : experience.name,
                experienceKey: experience === null || experience === void 0 ? void 0 : experience.key
            }, variation);
        }
        return bucketedVariation;
    };
    /**
     * @param {Id} experienceId
     * @param {Id} variationId
     * @return {Variation}
     * @private
     */
    DataManager.prototype.retrieveVariation = function (experienceId, variationId) {
        return this.getSubItem('experiences', experienceId, 'variations', variationId, 'id', 'id');
    };
    /**
     * @param {Id} visitorId
     * @param {StoreData} storeData
     * @private
     */
    DataManager.prototype.putLocalStore = function (visitorId, storeData) {
        var e_1, _a;
        var storeKey = this.getStoreKey(visitorId);
        this._bucketedVisitors.set(storeKey, storeData);
        if (this._bucketedVisitors.size > this._localStoreLimit) {
            try {
                // Delete one of the oldest record
                for (var _b = __values(this._bucketedVisitors), _c = _b.next(); !_c.done; _c = _b.next()) {
                    var _d = __read(_c.value, 2), key = _d[0], value = _d[1];
                    this._bucketedVisitors.delete(key);
                    break;
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
                }
                finally { if (e_1) throw e_1.error; }
            }
        }
    };
    /**
     * @param {Id} visitorId
     * @return {StoreData} variation id
     * @private
     */
    DataManager.prototype.getLocalStore = function (visitorId) {
        var storeKey = this.getStoreKey(visitorId);
        return this._bucketedVisitors.get(storeKey) || null;
    };
    /**
     * @param {Id} visitorId
     * @return {string} storeKey
     * @private
     */
    DataManager.prototype.getStoreKey = function (visitorId) {
        return "".concat(this._accountId, "-").concat(this._projectId, "-").concat(visitorId);
    };
    /**
     * Retrieve variation for visitor
     * @param {string} visitorId
     * @param {string} key
     * @param {Record<string, any> | null} visitorProperties
     * @param {Record<string, any> | null} locationProperties
     * @param {string=} environment
     * @return {BucketedVariation | RuleError}
     */
    DataManager.prototype.getBucketing = function (visitorId, key, visitorProperties, locationProperties, environment) {
        if (environment === void 0) { environment = this._environment; }
        return this._getBucketingByField(visitorId, key, visitorProperties, locationProperties, 'key', environment);
    };
    /**
     * Retrieve variation for Visitor
     * @param {string} visitorId
     * @param {Id} id
     * @param {Record<string, any> | null} visitorProperties
     * @param {Record<string, any> | null} locationProperties
     * @param {string=} environment
     * @return {BucketedVariation | RuleError}
     */
    DataManager.prototype.getBucketingById = function (visitorId, id, visitorProperties, locationProperties, environment) {
        if (environment === void 0) { environment = this._environment; }
        return this._getBucketingByField(visitorId, id, visitorProperties, locationProperties, 'id', environment);
    };
    /**
     * Process conversion event
     * @param {Id} visitorId
     * @param {Id} goalId
     * @param {Record<string, any>=} goalRule An object of key-value pairs that are used for goal matching
     * @param {Array<Record<GoalDataKey, number>>} goalData An array of object of key-value pairs
     * @param {SegmentsData} segments
     */
    DataManager.prototype.convert = function (visitorId, goalId, goalRule, goalData, segments) {
        var _a, _b, _c, _d, _e, _f;
        var goal = typeof goalId === 'string'
            ? this.getEntity(goalId, 'goals')
            : this.getEntityById(goalId, 'goals');
        if (!(goal === null || goal === void 0 ? void 0 : goal.id)) {
            (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.error) === null || _b === void 0 ? void 0 : _b.call(_a, jsSdkEnums.MESSAGES.GOAL_NOT_FOUND);
            return;
        }
        if (goalRule) {
            if (!(goal === null || goal === void 0 ? void 0 : goal.rules))
                return;
            var ruleMatched = this._ruleManager.isRuleMatched(goalRule, goal.rules);
            // Return rule errors if present
            if (Object.values(jsSdkEnums.RuleError).includes(ruleMatched))
                return ruleMatched;
            if (!ruleMatched) {
                (_d = (_c = this._loggerManager) === null || _c === void 0 ? void 0 : _c.error) === null || _d === void 0 ? void 0 : _d.call(_c, jsSdkEnums.MESSAGES.GOAL_RULE_NOT_MATCH);
                return;
            }
        }
        var data = {
            goalId: goal.id
        };
        var bucketingData = (this.getLocalStore(visitorId) || {}).bucketing;
        if (bucketingData)
            data.bucketingData = bucketingData;
        var event = {
            eventType: jsSdkEnums.EventType.CONVERSION,
            data: data
        };
        this._apiManager.enqueue(visitorId, event, segments);
        // Split transaction events
        if (goalData) {
            var data_1 = {
                goalId: goal.id,
                goalData: goalData
            };
            if (bucketingData)
                data_1.bucketingData = bucketingData;
            var event_1 = {
                eventType: jsSdkEnums.EventType.CONVERSION,
                data: data_1
            };
            this._apiManager.enqueue(visitorId, event_1, segments);
        }
        (_f = (_e = this._loggerManager) === null || _e === void 0 ? void 0 : _e.trace) === null || _f === void 0 ? void 0 : _f.call(_e, 'DataManager.convert()', {
            event: event
        });
    };
    /**
     * Get audiences that meet the visitorProperties
     * @param {Array<Record<any, any>>} items
     * @param {Record<string, any>} visitorProperties
     * @return {Array<Record<string, any> | RuleError>}
     */
    DataManager.prototype.filterMatchedRecordsWithRule = function (items, visitorProperties) {
        var _a, _b, _c, _d, _e;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'DataManager.filterMatchedRecordsWithRule()', {
            items: items,
            visitorProperties: visitorProperties
        });
        var matchedRecords = [];
        var match;
        if (jsSdkUtils.arrayNotEmpty(items)) {
            for (var i = 0, length_1 = items.length; i < length_1; i++) {
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
    };
    /**
     * Get audiences that meet the custom segments
     * @param {Array<Record<any, any>>} items
     * @param {Id} visitorId
     * @return {Array<Record<string, any>>}
     */
    DataManager.prototype.filterMatchedCustomSegments = function (items, visitorId) {
        var _a, _b, _c, _d, _e;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'DataManager.filterMatchedCustomSegments()', {
            items: items,
            visitorId: visitorId
        });
        // Check that custom segments are matched
        var storeData = this.getLocalStore(visitorId) || {};
        // Get custom segments ID from DataStore
        var _f = storeData, _g = _f.segments, _h = _g === void 0 ? {} : _g, _j = jsSdkEnums.SegmentsKeys.CUSTOM_SEGMENTS, _k = _h[_j], customSegments = _k === void 0 ? [] : _k;
        var matchedRecords = [];
        if (jsSdkUtils.arrayNotEmpty(items)) {
            for (var i = 0, length_2 = items.length; i < length_2; i++) {
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
    };
    /**
     * Get list of data entities
     * @param {string} entityType
     * @return {Array<Entity | Id>}
     */
    DataManager.prototype.getEntitiesList = function (entityType) {
        var _a, _b;
        var list = [];
        if (this._dataEntities.indexOf(entityType) !== -1) {
            list = jsSdkUtils.objectDeepValue(this._data, entityType) || [];
        }
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'DataManager.getEntitiesList()', {
            entityType: entityType,
            list: list
        });
        return list;
    };
    /**
     * Get list of data entities grouped by field
     * @param {string} entityType
     * @param {IdentityField=} field
     * @return {Record<string, Entity>}
     */
    DataManager.prototype.getEntitiesListObject = function (entityType, field) {
        if (field === void 0) { field = 'id'; }
        return this.getEntitiesList(entityType).reduce(function (target, entity) {
            target[entity[field]] = entity;
            return target;
        }, {});
    };
    /**
     *
     * @param {string|Id} identity Value of the field which name is provided in identityField
     * @param {string} entityType
     * @param {IdentityField=} identityField Defaults to 'key'
     * @return {Entity}
     * @private
     */
    DataManager.prototype._getEntityByField = function (identity, entityType, identityField) {
        var _a, _b, _c, _d;
        if (identityField === void 0) { identityField = 'key'; }
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'DataManager._getEntityByField()', {
            identity: identity,
            entityType: entityType,
            identityField: identityField
        });
        var list = this.getEntitiesList(entityType);
        if (jsSdkUtils.arrayNotEmpty(list)) {
            for (var i = 0, length_3 = list.length; i < length_3; i++) {
                if (list[i] &&
                    ((_d = (_c = list[i]) === null || _c === void 0 ? void 0 : _c[identityField]) === null || _d === void 0 ? void 0 : _d.toString()) === identity.toString()) {
                    return list[i];
                }
            }
        }
        return null;
    };
    /**
     * Find the entity in list by id
     * @param {string} key
     * @param {string} entityType
     * @return {Entity}
     */
    DataManager.prototype.getEntity = function (key, entityType) {
        return this._getEntityByField(key, entityType, 'key');
    };
    /**
     * Find the entity in list by keys
     * @param {Array<string>} keys
     * @param {string} entityType
     * @return {Array<Entity>}
     */
    DataManager.prototype.getEntities = function (keys, entityType) {
        return this.getItemsByKeys(keys, entityType);
    };
    /**
     * Find the entity in list by id
     * @param {Id} id
     * @param {string} entityType
     * @return {Entity}
     */
    DataManager.prototype.getEntityById = function (id, entityType) {
        return this._getEntityByField(id, entityType, 'id');
    };
    /**
     * Find the entity in list by ids
     * @param {Array<Id>} ids
     * @param {string} entityType
     * @return {Array<Entity>}
     */
    DataManager.prototype.getEntitiesByIds = function (ids, entityType) {
        return this.getItemsByIds(ids, entityType);
    };
    /**
     * Find the items in list by  keys
     * @param {Array<string>} keys
     * @param {string} path
     * @return {Array<Record<string, any>>}
     */
    DataManager.prototype.getItemsByKeys = function (keys, path) {
        var _a;
        var list = this.getEntitiesList(path);
        var items = [];
        if (jsSdkUtils.arrayNotEmpty(list)) {
            for (var i = 0, length_4 = list.length; i < length_4; i++) {
                if (keys.indexOf((_a = list[i]) === null || _a === void 0 ? void 0 : _a.key) !== -1) {
                    items.push(list[i]);
                }
            }
        }
        return items;
    };
    /**
     * Find the items in list by ids
     * @param {Array<Id>} ids
     * @param {String} path
     * @return {Array<Record<string, any>>}
     */
    DataManager.prototype.getItemsByIds = function (ids, path) {
        var _a, _b, _c, _d;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'DataManager.getItemsByIds()', {
            ids: ids,
            path: path
        });
        var items = [];
        if (jsSdkUtils.arrayNotEmpty(ids)) {
            var list = this.getEntitiesList(path);
            if (jsSdkUtils.arrayNotEmpty(list)) {
                for (var i = 0, length_5 = list.length; i < length_5; i++) {
                    if (ids.indexOf(Number((_c = list[i]) === null || _c === void 0 ? void 0 : _c.id)) !== -1 ||
                        ids.indexOf(String((_d = list[i]) === null || _d === void 0 ? void 0 : _d.id)) !== -1) {
                        items.push(list[i]);
                    }
                }
            }
        }
        return items;
    };
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
    DataManager.prototype.getSubItem = function (entityType, entityIdentity, subEntityType, subEntityIdentity, identityField, subIdentityField) {
        var _a;
        var entity = this._getEntityByField(entityIdentity, entityType, identityField);
        for (var k in entity[subEntityType]) {
            if (((_a = entity[subEntityType][k]) === null || _a === void 0 ? void 0 : _a[subIdentityField]) == subEntityIdentity) {
                return entity[subEntityType][k];
            }
        }
        return null;
    };
    /**
     * Validates data object
     * @param data
     * @return {boolean}
     */
    DataManager.prototype.isValidConfigData = function (data) {
        var _a;
        return jsSdkUtils.objectNotEmpty(data) && !!(data === null || data === void 0 ? void 0 : data.account_id) && !!((_a = data === null || data === void 0 ? void 0 : data.project) === null || _a === void 0 ? void 0 : _a.id);
    };
    return DataManager;
}());

exports.DataManager = DataManager;
exports.DataStoreManager = DataStoreManager;
//# sourceMappingURL=index.js.map
