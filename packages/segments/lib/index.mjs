import { SegmentsKeys, RuleError, MESSAGES } from '@convertcom/js-sdk-enums';
import { objectDeepValue } from '@convertcom/js-sdk-utils';

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
class SegmentsManager {
    /**
     * @param {Config} config
     * @param {Object} dependencies
     * @param {DataManagerInterface=} dependencies.dataManager
     * @param {RuleManagerInterface=} dependencies.ruleManager
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    constructor(config, { dataManager, ruleManager, loggerManager }) {
        this._dataManager = dataManager;
        this._ruleManager = ruleManager;
        this._loggerManager = loggerManager;
        this._data = objectDeepValue(config, 'data');
    }
    /**
     * Get segments in DataStore
     * @param {Id} visitorId
     * @returns {SegmentsData}
     */
    getSegments(visitorId) {
        const storeData = this._dataManager.getLocalStore(visitorId) || {};
        return storeData === null || storeData === void 0 ? void 0 : storeData.segments;
    }
    /**
     * Update segments in DataStore
     * @param {Id} visitorId
     * @param {SegmentsData} segments
     */
    putSegments(visitorId, segments) {
        // Store the data in local variable
        this._dataManager.putLocalStore(visitorId, { segments });
        // Enqueue to store in dataStore
        const storeKey = this._dataManager.getStoreKey(visitorId);
        this._dataManager.dataStoreManager.enqueue(storeKey, { segments });
    }
    setSegments(visitorId, segments, segmentRule) {
        var _a, _b, _c, _d, _e;
        const storeData = this._dataManager.getLocalStore(visitorId) || {};
        // Get custom segments ID from DataStore
        const { segments: { [SegmentsKeys.CUSTOM_SEGMENTS]: customSegments = [] } = {} } = storeData;
        const segmentIds = [];
        let segmentsMatched = false;
        for (const segment of segments) {
            if (segmentRule && !segmentsMatched) {
                segmentsMatched = this._ruleManager.isRuleMatched(segmentRule, segment === null || segment === void 0 ? void 0 : segment.rules);
                // Return rule errors if present
                if (Object.values(RuleError).includes(segmentsMatched))
                    return segmentsMatched;
            }
            if (!segmentRule || segmentsMatched) {
                const segmentId = (_a = segment === null || segment === void 0 ? void 0 : segment.id) === null || _a === void 0 ? void 0 : _a.toString();
                if (customSegments.includes(segmentId)) {
                    (_c = (_b = this._loggerManager) === null || _b === void 0 ? void 0 : _b.warn) === null || _c === void 0 ? void 0 : _c.call(_b, MESSAGES.CUSTOM_SEGMENTS_KEY_FOUND);
                }
                else {
                    segmentIds.push(segmentId);
                }
            }
        }
        let segmentsData;
        if (segmentIds.length) {
            segmentsData = Object.assign(Object.assign({}, (storeData.segments || {})), { [SegmentsKeys.CUSTOM_SEGMENTS]: [...customSegments, ...segmentIds] });
            // Merge custom segments ID into DataStore
            this.putSegments(visitorId, segmentsData);
        }
        else {
            (_e = (_d = this._loggerManager) === null || _d === void 0 ? void 0 : _d.error) === null || _e === void 0 ? void 0 : _e.call(_d, MESSAGES.SEGMENTS_NOT_FOUND);
        }
        return segmentsData;
    }
    /**
     * Update custom segments for specific visitor
     * @param {Id} visitorId
     * @param {Array<string>} segmentKeys A list of segment keys
     * @param {Record<string, any>=} segmentRule An object of key-value pairs that are used for segments matching
     * @param {string=} environment
     * @return {SegmentsData | RuleError}
     */
    selectCustomSegments(visitorId, segmentKeys, segmentRule) {
        const segments = this._dataManager.getEntities(segmentKeys, 'segments');
        return this.setSegments(visitorId, segments, segmentRule);
    }
    /**
     * Update custom segments for specific visitor
     * @param {Id} visitorId
     * @param {Array<Id>} segmentIds A list of segment keys
     * @param {Record<string, any>=} segmentRule An object of key-value pairs that are used for segments matching
     * @param {string=} environment
     * @return {SegmentsData | RuleError}
     */
    selectCustomSegmentsByIds(visitorId, segmentIds, segmentRule) {
        const segments = this._dataManager.getEntitiesByIds(segmentIds, 'segments');
        return this.setSegments(visitorId, segments, segmentRule);
    }
}

export { SegmentsManager };
//# sourceMappingURL=index.mjs.map
