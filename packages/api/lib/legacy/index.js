'use strict';

var jsSdkEnums = require('@convertcom/js-sdk-enums');
var jsSdkUtils = require('@convertcom/js-sdk-utils');

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

function __awaiter(thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
}

function __generator(thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (g && (g = 0, op[0] && (_ = 0)), _) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
}

typeof SuppressedError === "function" ? SuppressedError : function (error, suppressed, message) {
    var e = new Error(message);
    return e.name = "SuppressedError", e.error = error, e.suppressed = suppressed, e;
};

/*!
 * Convert JS SDK config
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
var DEFAULT_CONFIG_ENDPOINT = process.env.CONFIG_ENDPOINT || 'https://cdn-4.convertexperiments.com/api/v1/';
var DEFAULT_TRACK_ENDPOINT = process.env.TRACK_ENDPOINT ||
    'https://[project_id].metrics.convertexperiments.com/v1/';

var DEFAULT_HEADERS = {
    'Content-type': 'application/json'
};
var DEFAULT_BATCH_SIZE = 10;
var DEFAULT_RELEASE_INTERVAL = 10000;
/**
 * Provides logic for network requests. Reads remote configuration and sends tracking events to Convert server.
 * @category Modules
 * @constructor
 * @implements {ApiManagerInterface}
 */
var ApiManager = /** @class */ (function () {
    /**
     * @param {Config=} config
     * @param {Object=} dependencies
     * @param {EventManagerInterface=} dependencies.eventManager
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    function ApiManager(config, _a) {
        var _b = _a === void 0 ? {} : _a, eventManager = _b.eventManager, loggerManager = _b.loggerManager;
        var _c, _d, _e;
        this._configEndpoint = DEFAULT_CONFIG_ENDPOINT;
        this._trackEndpoint = DEFAULT_TRACK_ENDPOINT;
        this._defaultHeaders = DEFAULT_HEADERS;
        this.batchSize = DEFAULT_BATCH_SIZE;
        this.releaseInterval = DEFAULT_RELEASE_INTERVAL;
        this._loggerManager = loggerManager;
        this._eventManager = eventManager;
        this._configEndpoint = jsSdkUtils.objectDeepValue(config, 'api.endpoint.config', DEFAULT_CONFIG_ENDPOINT);
        this._trackEndpoint = jsSdkUtils.objectDeepValue(config, 'api.endpoint.track', DEFAULT_TRACK_ENDPOINT);
        this._data = jsSdkUtils.objectDeepValue(config, 'data');
        this._enrichData = !(config === null || config === void 0 ? void 0 : config.dataStore);
        this.batchSize =
            Number(jsSdkUtils.objectDeepValue(config, 'events.batch_size')).valueOf() ||
                DEFAULT_BATCH_SIZE;
        this.releaseInterval =
            Number(jsSdkUtils.objectDeepValue(config, 'events.release_interval')).valueOf() ||
                DEFAULT_RELEASE_INTERVAL;
        this._accountId = (_c = this._data) === null || _c === void 0 ? void 0 : _c.account_id;
        this._projectId = (_e = (_d = this._data) === null || _d === void 0 ? void 0 : _d.project) === null || _e === void 0 ? void 0 : _e.id;
        this._trackingEvent = {
            enrichData: this._enrichData,
            accountId: this._accountId,
            projectId: this._projectId,
            visitors: []
        };
        this._requestsQueue = {
            length: 0,
            items: [],
            push: function (visitorId, eventRequest, segments) {
                var visitorIndex = this.items.findIndex(function (event) { return event.visitorId === visitorId; });
                if (visitorIndex !== -1) {
                    this.items[visitorIndex].events.push(eventRequest);
                }
                else {
                    var visitor = {
                        visitorId: visitorId,
                        events: [eventRequest]
                    };
                    if (segments)
                        visitor.segments = segments;
                    this.items.push(visitor);
                }
                this.length++;
            },
            reset: function () {
                this.items = [];
                this.length = 0;
            }
        };
    }
    /**
     * Send request to api server
     * @param method
     * @param path
     * @param data
     * @param headers
     * @return {Promise<HttpResponse>}
     */
    ApiManager.prototype.request = function (method, path, data, headers) {
        if (data === void 0) { data = {}; }
        if (headers === void 0) { headers = {}; }
        return __awaiter(this, void 0, void 0, function () {
            var requestHeaders, requestConfig;
            return __generator(this, function (_a) {
                requestHeaders = __assign(__assign({}, this._defaultHeaders), headers);
                requestConfig = {
                    method: method,
                    path: path.route,
                    baseURL: path.base,
                    headers: requestHeaders,
                    data: data,
                    responseType: 'json'
                };
                return [2 /*return*/, jsSdkUtils.HttpClient.request(requestConfig)];
            });
        });
    };
    /**
     * Add request to queue for sending to server
     * @param {Id} visitorId
     * @param {VisitorEvent} eventRequest
     * @param {SegmentsData} segments
     */
    ApiManager.prototype.enqueue = function (visitorId, eventRequest, segments) {
        var _a, _b;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'ApiManager.enqueue()', {
            eventRequest: eventRequest
        });
        this._requestsQueue.push(visitorId, eventRequest, segments);
        if (this._requestsQueue.length === this.batchSize) {
            this.releaseQueue('size').then();
        }
        else {
            if (this._requestsQueue.length === 1) {
                this.startQueue();
            }
        }
    };
    /**
     * Send queue to server
     * @param {string=} reason
     * @return {Promise<any>}
     */
    ApiManager.prototype.releaseQueue = function (reason) {
        var _this = this;
        var _a, _b;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'ApiManager.releaseQueue()', {
            reason: reason || ''
        });
        this.stopQueue();
        var payload = this._trackingEvent;
        payload.visitors = this._requestsQueue.items.slice();
        return this.request('post', {
            base: this._trackEndpoint.replace('[project_id]', this._projectId.toString()),
            route: "/track/".concat(this._accountId, "/").concat(this._projectId)
        }, payload)
            .then(function (result) {
            var _a, _b;
            _this._requestsQueue.reset();
            (_b = (_a = _this._eventManager) === null || _a === void 0 ? void 0 : _a.fire) === null || _b === void 0 ? void 0 : _b.call(_a, jsSdkEnums.SystemEvents.API_QUEUE_RELEASED, {
                reason: reason,
                result: result
            });
        })
            .catch(function (error) {
            var _a, _b, _c, _d;
            // TODO: set an exponential backoff
            (_b = (_a = _this._loggerManager) === null || _a === void 0 ? void 0 : _a.error) === null || _b === void 0 ? void 0 : _b.call(_a, 'ApiManager.releaseQueue()', {
                error: error.message
            });
            _this.startQueue();
            (_d = (_c = _this._eventManager) === null || _c === void 0 ? void 0 : _c.fire) === null || _d === void 0 ? void 0 : _d.call(_c, jsSdkEnums.SystemEvents.API_QUEUE_RELEASED, { reason: reason }, error);
        });
    };
    /**
     * Stop queue timer
     */
    ApiManager.prototype.stopQueue = function () {
        clearTimeout(this._requestsQueueTimerID);
    };
    /**
     * Start queue timer
     */
    ApiManager.prototype.startQueue = function () {
        var _this = this;
        this._requestsQueueTimerID = setTimeout(function () {
            _this.releaseQueue('timeout');
        }, this.releaseInterval);
    };
    /**
     * Set data
     */
    ApiManager.prototype.setData = function (data) {
        var _a;
        this._data = data;
        this._accountId = data === null || data === void 0 ? void 0 : data.account_id;
        this._projectId = (_a = data === null || data === void 0 ? void 0 : data.project) === null || _a === void 0 ? void 0 : _a.id;
        this._trackingEvent.accountId = this._accountId;
        this._trackingEvent.projectId = this._projectId;
    };
    /**
     * Get config data by SDK key
     * @param {string} sdkKey
     * @return {Promise<ConfigData>}
     */
    ApiManager.prototype.getConfigByKey = function (sdkKey) {
        var _this = this;
        var _a, _b;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'ApiManager.getConfigByKey()', {
            sdkKey: sdkKey
        });
        return new Promise(function (resolve, reject) {
            _this.request('get', {
                base: _this._configEndpoint,
                route: "/config/".concat(sdkKey)
            })
                .then(function (_a) {
                var data = _a.data;
                return resolve(data);
            })
                .catch(reject);
        });
    };
    return ApiManager;
}());

exports.ApiManager = ApiManager;
//# sourceMappingURL=index.js.map
