'use strict';

var Murmurhash = require('murmurhash');
var jsSdkUtils = require('@convertcom/js-sdk-utils');
var jsSdkEnums = require('@convertcom/js-sdk-enums');

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
//default hash seed
const DEFAULT_HASH_SEED = 9999;
const DEFAULT_MAX_TRAFFIC = 10000;
const DEFAULT_MAX_HASH = 4294967296;
/**
 * Provides logic for bucketing for specific visitor (by visitorId) or randomly
 * @category Modules
 * @constructor
 * @implements {BucketingManagerInterface}
 */
class BucketingManager {
    /**
     * @param {Config=} config
     * @param {Object=} dependencies
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    constructor(config, { loggerManager } = {}) {
        var _a, _b;
        this._max_traffic = DEFAULT_MAX_TRAFFIC;
        this._hash_seed = DEFAULT_HASH_SEED;
        this._loggerManager = loggerManager;
        this._max_traffic = jsSdkUtils.objectDeepValue(config, 'bucketing.max_traffic', DEFAULT_MAX_TRAFFIC, true);
        this._hash_seed = jsSdkUtils.objectDeepValue(config, 'bucketing.hash_seed', DEFAULT_HASH_SEED, true);
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, jsSdkEnums.MESSAGES.BUCKETING_CONSTRUCTOR, this);
    }
    /**
     * Select variation based on its percentages and value provided
     * @param {object} buckets Key-value object with variations IDs as keys and percentages as values
     * @param {number} value A bucket value
     * @param {number=} redistribute Defaults to '0'
     * @return {string | null}
     */
    selectBucket(buckets, value, redistribute = 0) {
        var _a, _b;
        let variation = null;
        let prev = 0;
        Object.keys(buckets).some((id) => {
            prev += buckets[id] * 100 + redistribute;
            if (value < prev) {
                variation = id;
                return true;
            }
            return false;
        });
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.debug) === null || _b === void 0 ? void 0 : _b.call(_a, 'BucketingManager.selectBucket()', {
            buckets: buckets,
            value: value,
            redistribute: redistribute
        }, { variation: variation });
        return variation || null;
    }
    /**
     * Get a value based on hash from Visitor id to use for bucket selecting
     * @param {Id} visitorId
     * @param {number=} seed
     * @return {number}
     */
    getValueVisitorBased(visitorId, seed = this._hash_seed) {
        var _a, _b;
        const hash = Murmurhash.v3(String(visitorId), seed);
        const val = (hash / DEFAULT_MAX_HASH) * this._max_traffic;
        const result = parseInt(String(val), 10);
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.debug) === null || _b === void 0 ? void 0 : _b.call(_a, 'BucketingManager.getValueVisitorBased()', {
            visitorId: visitorId,
            seed: seed,
            val: val,
            result: result
        });
        return result;
    }
    /**
     * Get a bucket for the visitor
     * @param {object} buckets Key-value object with variations IDs as keys and percentages as values
     * @param {Id} visitorId
     * @param {number} [redistribute=0]
     * @param {number} [seed=]
     * @return {string | null}
     */
    getBucketForVisitor(buckets, visitorId, redistribute = 0, seed) {
        const value = this.getValueVisitorBased(visitorId, seed);
        return this.selectBucket(buckets, value, redistribute);
    }
}

exports.BucketingManager = BucketingManager;
//# sourceMappingURL=index.js.map
