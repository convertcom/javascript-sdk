'use strict';

var utils = require('@convertcom/utils');
var enums = require('@convertcom/enums');

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
        this._requestsQueue = utils.objectDeepMerge(this._requestsQueue, addData);
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
        (_d = (_c = this._eventManager) === null || _c === void 0 ? void 0 : _c.fire) === null || _d === void 0 ? void 0 : _d.call(_c, enums.SystemEvents.DATA_STORE_QUEUE_RELEASED, {
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
        if (this.isValidDataStore(dataStore)) {
            this._dataStore = dataStore;
        }
        else {
            (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.error) === null || _b === void 0 ? void 0 : _b.call(_a, enums.ERROR_MESSAGES.DATA_STORE_NOT_VALID);
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
        return (dataStore &&
            typeof dataStore === 'object' &&
            typeof dataStore['get'] === 'function' &&
            typeof dataStore['set'] === 'function');
    }
}

exports.DataStoreManager = DataStoreManager;
//# sourceMappingURL=index.js.map
