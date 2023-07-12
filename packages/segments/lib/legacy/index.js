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

function __spreadArray(to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
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
/**
 * Provides segments specific logic
 * @category Modules
 * @constructor
 * @implements {SegmentsManagerInterface}
 */
var SegmentsManager = /** @class */ (function () {
    /**
     * @param {Config} config
     * @param {Object} dependencies
     * @param {DataManagerInterface=} dependencies.dataManager
     * @param {RuleManagerInterface=} dependencies.ruleManager
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    function SegmentsManager(config, _a) {
        var dataManager = _a.dataManager, ruleManager = _a.ruleManager, loggerManager = _a.loggerManager;
        this._dataManager = dataManager;
        this._ruleManager = ruleManager;
        this._loggerManager = loggerManager;
        this._data = jsSdkUtils.objectDeepValue(config, 'data');
    }
    /**
     * Get segments in DataStore
     * @param {Id} visitorId
     * @returns {SegmentsData}
     */
    SegmentsManager.prototype.getSegments = function (visitorId) {
        var storeData = this._dataManager.getLocalStore(visitorId) || {};
        return storeData === null || storeData === void 0 ? void 0 : storeData.segments;
    };
    /**
     * Update segments in DataStore
     * @param {Id} visitorId
     * @param {SegmentsData} segments
     */
    SegmentsManager.prototype.putSegments = function (visitorId, segments) {
        // Store the data in local variable
        this._dataManager.putLocalStore(visitorId, { segments: segments });
        // Enqueue to store in dataStore
        var storeKey = this._dataManager.getStoreKey(visitorId);
        this._dataManager.dataStoreManager.enqueue(storeKey, { segments: segments });
    };
    SegmentsManager.prototype.setCustomSegments = function (visitorId, segments, segmentRule) {
        var e_1, _a, _b;
        var _c, _d, _e, _f, _g;
        var storeData = this._dataManager.getLocalStore(visitorId) || {};
        // Get custom segments ID from DataStore
        var _h = storeData, _j = _h.segments, _k = _j === void 0 ? {} : _j, _l = jsSdkEnums.SegmentsKeys.CUSTOM_SEGMENTS, _m = _k[_l], customSegments = _m === void 0 ? [] : _m;
        var segmentIds = [];
        var segmentsMatched = false;
        try {
            for (var segments_1 = __values(segments), segments_1_1 = segments_1.next(); !segments_1_1.done; segments_1_1 = segments_1.next()) {
                var segment = segments_1_1.value;
                if (segmentRule && !segmentsMatched) {
                    segmentsMatched = this._ruleManager.isRuleMatched(segmentRule, segment === null || segment === void 0 ? void 0 : segment.rules);
                    // Return rule errors if present
                    if (Object.values(jsSdkEnums.RuleError).includes(segmentsMatched))
                        return segmentsMatched;
                }
                if (!segmentRule || segmentsMatched) {
                    var segmentId = (_c = segment === null || segment === void 0 ? void 0 : segment.id) === null || _c === void 0 ? void 0 : _c.toString();
                    if (customSegments.includes(segmentId)) {
                        (_e = (_d = this._loggerManager) === null || _d === void 0 ? void 0 : _d.warn) === null || _e === void 0 ? void 0 : _e.call(_d, jsSdkEnums.MESSAGES.CUSTOM_SEGMENTS_KEY_FOUND);
                    }
                    else {
                        segmentIds.push(segmentId);
                    }
                }
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (segments_1_1 && !segments_1_1.done && (_a = segments_1.return)) _a.call(segments_1);
            }
            finally { if (e_1) throw e_1.error; }
        }
        var segmentsData;
        if (segmentIds.length) {
            segmentsData = __assign(__assign({}, (storeData.segments || {})), (_b = {}, _b[jsSdkEnums.SegmentsKeys.CUSTOM_SEGMENTS] = __spreadArray(__spreadArray([], __read(customSegments), false), __read(segmentIds), false), _b));
            // Merge custom segments ID into DataStore
            this.putSegments(visitorId, segmentsData);
        }
        else {
            (_g = (_f = this._loggerManager) === null || _f === void 0 ? void 0 : _f.warn) === null || _g === void 0 ? void 0 : _g.call(_f, jsSdkEnums.MESSAGES.SEGMENTS_NOT_FOUND);
        }
        return segmentsData;
    };
    /**
     * Update custom segments for specific visitor
     * @param {Id} visitorId
     * @param {Array<string>} segmentKeys A list of segment keys
     * @param {Record<string, any>=} segmentRule An object of key-value pairs that are used for segments matching
     * @param {string=} environment
     * @return {SegmentsData | RuleError}
     */
    SegmentsManager.prototype.selectCustomSegments = function (visitorId, segmentKeys, segmentRule) {
        var segments = this._dataManager.getEntities(segmentKeys, 'segments');
        return this.setCustomSegments(visitorId, segments, segmentRule);
    };
    /**
     * Update custom segments for specific visitor
     * @param {Id} visitorId
     * @param {Array<Id>} segmentIds A list of segment ids
     * @param {Record<string, any>=} segmentRule An object of key-value pairs that are used for segments matching
     * @param {string=} environment
     * @return {SegmentsData | RuleError}
     */
    SegmentsManager.prototype.selectCustomSegmentsByIds = function (visitorId, segmentIds, segmentRule) {
        var segments = this._dataManager.getEntitiesByIds(segmentIds, 'segments');
        return this.setCustomSegments(visitorId, segments, segmentRule);
    };
    return SegmentsManager;
}());

exports.SegmentsManager = SegmentsManager;
//# sourceMappingURL=index.js.map
