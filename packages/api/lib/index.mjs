import { SystemEvents } from '@convertcom/enums';
import { objectDeepValue, HttpClient } from '@convertcom/utils';

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
/* global Reflect, Promise */


function __awaiter(thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
}

/*!
 * Convert JS SDK config
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
const DEFAULT_CONFIG_ENDPOINT = process.env.CONFIG_ENDPOINT || 'https://cdn-4.convertexperiments.com/api/v1/';
const DEFAULT_TRACK_ENDPOINT = process.env.TRACK_ENDPOINT ||
    'https://[project_id].metrics.convertexperiments.com/v1/';

const DEFAULT_HEADERS = {
    'Content-type': 'application/json'
};
const DEFAULT_BATCH_SIZE = 10;
const DEFAULT_RELEASE_INTERVAL = 10000;
/**
 * Provides logic for network requests. Reads remote configuration and sends tracking events to Convert server.
 * @category Modules
 * @constructor
 * @implements {ApiManagerInterface}
 */
class ApiManager {
    /**
     * @param {Config=} config
     * @param {Object=} dependencies
     * @param {EventManagerInterface=} dependencies.eventManager
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    constructor(config, { eventManager, loggerManager } = {}) {
        var _a, _b, _c;
        this._configEndpoint = DEFAULT_CONFIG_ENDPOINT;
        this._trackEndpoint = DEFAULT_TRACK_ENDPOINT;
        this._defaultHeaders = DEFAULT_HEADERS;
        this.batchSize = DEFAULT_BATCH_SIZE;
        this.releaseInterval = DEFAULT_RELEASE_INTERVAL;
        this._loggerManager = loggerManager;
        this._eventManager = eventManager;
        this._configEndpoint = objectDeepValue(config, 'api.endpoint.config', DEFAULT_CONFIG_ENDPOINT);
        this._trackEndpoint = objectDeepValue(config, 'api.endpoint.track', DEFAULT_TRACK_ENDPOINT);
        this._data = objectDeepValue(config, 'data');
        this._enrichData = !(config === null || config === void 0 ? void 0 : config.dataStore);
        this.batchSize =
            Number(objectDeepValue(config, 'events.batch_size')).valueOf() ||
                DEFAULT_BATCH_SIZE;
        this.releaseInterval =
            Number(objectDeepValue(config, 'events.release_interval')).valueOf() ||
                DEFAULT_RELEASE_INTERVAL;
        this._accountId = (_a = this._data) === null || _a === void 0 ? void 0 : _a.account_id;
        this._projectId = (_c = (_b = this._data) === null || _b === void 0 ? void 0 : _b.project) === null || _c === void 0 ? void 0 : _c.id;
        this._trackingEvent = {
            enrichData: this._enrichData,
            accountId: this._accountId,
            projectId: this._projectId,
            visitors: []
        };
        this._requestsQueue = {
            length: 0,
            items: [],
            push(visitorId, eventRequest, segments) {
                const visitorIndex = this.items.findIndex((event) => event.visitorId === visitorId);
                if (visitorIndex !== -1) {
                    this.items[visitorIndex].events.push(eventRequest);
                }
                else {
                    const visitor = {
                        visitorId,
                        events: [eventRequest]
                    };
                    if (segments)
                        visitor.segments = segments;
                    this.items.push(visitor);
                }
                this.length++;
            },
            reset() {
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
    request(method, path, data = {}, headers = {}) {
        return __awaiter(this, void 0, void 0, function* () {
            const requestHeaders = Object.assign(Object.assign({}, this._defaultHeaders), headers);
            const requestConfig = {
                method: method,
                path: path.route,
                baseURL: path.base,
                headers: requestHeaders,
                data: data,
                responseType: 'json'
            };
            return HttpClient.request(requestConfig);
        });
    }
    /**
     * Add request to queue for sending to server
     * @param {Id} visitorId
     * @param {VisitorEvent} eventRequest
     * @param {SegmentsData} segments
     */
    enqueue(visitorId, eventRequest, segments) {
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
    }
    /**
     * Send queue to server
     * @param {string=} reason
     * @return {Promise<any>}
     */
    releaseQueue(reason) {
        var _a, _b;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'ApiManager.releaseQueue()', {
            reason: reason || ''
        });
        this.stopQueue();
        const payload = this._trackingEvent;
        payload.visitors = this._requestsQueue.items.slice();
        return this.request('post', {
            base: this._trackEndpoint.replace('[project_id]', this._projectId.toString()),
            route: `/track/${this._accountId}/${this._projectId}`
        }, payload)
            .then((result) => {
            var _a, _b;
            this._requestsQueue.reset();
            (_b = (_a = this._eventManager) === null || _a === void 0 ? void 0 : _a.fire) === null || _b === void 0 ? void 0 : _b.call(_a, SystemEvents.API_QUEUE_RELEASED, {
                reason: reason,
                result: result
            });
        })
            .catch((error) => {
            var _a, _b, _c, _d;
            // TODO: set an exponential backoff
            (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.error) === null || _b === void 0 ? void 0 : _b.call(_a, 'ApiManager.releaseQueue()', {
                error: error.message
            });
            this.startQueue();
            (_d = (_c = this._eventManager) === null || _c === void 0 ? void 0 : _c.fire) === null || _d === void 0 ? void 0 : _d.call(_c, SystemEvents.API_QUEUE_RELEASED, { reason: reason }, error);
        });
    }
    /**
     * Stop queue timer
     */
    stopQueue() {
        clearTimeout(this._requestsQueueTimerID);
    }
    /**
     * Start queue timer
     */
    startQueue() {
        this._requestsQueueTimerID = setTimeout(() => {
            this.releaseQueue('timeout');
        }, this.releaseInterval);
    }
    /**
     * Set data
     */
    setData(data) {
        var _a;
        this._data = data;
        this._accountId = data === null || data === void 0 ? void 0 : data.account_id;
        this._projectId = (_a = data === null || data === void 0 ? void 0 : data.project) === null || _a === void 0 ? void 0 : _a.id;
        this._trackingEvent.accountId = this._accountId;
        this._trackingEvent.projectId = this._projectId;
    }
    /**
     * Get config data by SDK key
     * @param {string} sdkKey
     * @return {Promise<ConfigData>}
     */
    getConfigByKey(sdkKey) {
        var _a, _b;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'ApiManager.getConfigByKey()', {
            sdkKey
        });
        return new Promise((resolve, reject) => {
            this.request('get', {
                base: this._configEndpoint,
                route: `/config/${sdkKey}`
            })
                .then(({ data }) => resolve(data))
                .catch(reject);
        });
    }
}

export { ApiManager };
//# sourceMappingURL=index.mjs.map
