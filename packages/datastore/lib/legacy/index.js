'use strict';

var utils = require('@convertcom/utils');
var enums = require('@convertcom/enums');

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
        this._requestsQueue = utils.objectDeepMerge(this._requestsQueue, addData);
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
        (_d = (_c = this._eventManager) === null || _c === void 0 ? void 0 : _c.fire) === null || _d === void 0 ? void 0 : _d.call(_c, enums.SystemEvents.DATA_STORE_QUEUE_RELEASED, {
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
            if (this.isValidDataStore(dataStore)) {
                this._dataStore = dataStore;
            }
            else {
                (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.error) === null || _b === void 0 ? void 0 : _b.call(_a, enums.ERROR_MESSAGES.DATA_STORE_NOT_VALID);
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
        return (dataStore &&
            typeof dataStore === 'object' &&
            typeof dataStore['get'] === 'function' &&
            typeof dataStore['set'] === 'function');
    };
    return DataStoreManager;
}());

exports.DataStoreManager = DataStoreManager;
//# sourceMappingURL=index.js.map
