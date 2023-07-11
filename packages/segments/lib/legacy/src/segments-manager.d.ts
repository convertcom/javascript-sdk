/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import { SegmentsManagerInterface } from './interfaces/segments-manager';
import { Config, Id, SegmentsData } from '@convertcom/js-sdk-types';
import { RuleError } from '@convertcom/js-sdk-enums';
import { LogManagerInterface } from '@convertcom/js-sdk-logger';
import { DataManagerInterface } from '@convertcom/js-sdk-data';
import { RuleManagerInterface } from '@convertcom/js-sdk-rules';
/**
 * Provides segments specific logic
 * @category Modules
 * @constructor
 * @implements {SegmentsManagerInterface}
 */
export declare class SegmentsManager implements SegmentsManagerInterface {
    private _data;
    private _dataManager;
    private _ruleManager;
    private _loggerManager;
    /**
     * @param {Config} config
     * @param {Object} dependencies
     * @param {DataManagerInterface=} dependencies.dataManager
     * @param {RuleManagerInterface=} dependencies.ruleManager
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    constructor(config: Config, { dataManager, ruleManager, loggerManager }: {
        dataManager: DataManagerInterface;
        ruleManager: RuleManagerInterface;
        loggerManager?: LogManagerInterface;
    });
    /**
     * Get segments in DataStore
     * @param {Id} visitorId
     * @returns {SegmentsData}
     */
    getSegments(visitorId: Id): SegmentsData;
    /**
     * Update segments in DataStore
     * @param {Id} visitorId
     * @param {SegmentsData} segments
     */
    putSegments(visitorId: Id, segments: SegmentsData): void;
    private setCustomSegments;
    /**
     * Update custom segments for specific visitor
     * @param {Id} visitorId
     * @param {Array<string>} segmentKeys A list of segment keys
     * @param {Record<string, any>=} segmentRule An object of key-value pairs that are used for segments matching
     * @param {string=} environment
     * @return {SegmentsData | RuleError}
     */
    selectCustomSegments(visitorId: Id, segmentKeys: Array<string>, segmentRule?: Record<string, any>): SegmentsData | RuleError;
    /**
     * Update custom segments for specific visitor
     * @param {Id} visitorId
     * @param {Array<Id>} segmentIds A list of segment ids
     * @param {Record<string, any>=} segmentRule An object of key-value pairs that are used for segments matching
     * @param {string=} environment
     * @return {SegmentsData | RuleError}
     */
    selectCustomSegmentsByIds(visitorId: Id, segmentIds: Array<Id>, segmentRule?: Record<string, any>): SegmentsData | RuleError;
}
