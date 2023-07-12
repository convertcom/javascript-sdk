/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import { BucketingManagerInterface } from './interfaces/bucketing-manager';
import { Config, Id } from '@convertcom/js-sdk-types';
import { LogManagerInterface } from '@convertcom/js-sdk-logger';
/**
 * Provides logic for bucketing for specific visitor (by visitorId) or randomly
 * @category Modules
 * @constructor
 * @implements {BucketingManagerInterface}
 */
export declare class BucketingManager implements BucketingManagerInterface {
    private readonly _max_traffic;
    private readonly _hash_seed;
    private _loggerManager;
    /**
     * @param {Config=} config
     * @param {Object=} dependencies
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    constructor(config?: Config, { loggerManager }?: {
        loggerManager?: LogManagerInterface;
    });
    /**
     * Select variation based on its percentages and value provided
     * @param {object} buckets Key-value object with variations IDs as keys and percentages as values
     * @param {number} value A bucket value
     * @param {number=} redistribute Defaults to '0'
     * @return {string | null}
     */
    selectBucket(buckets: Record<string, number>, value: number, redistribute?: number): string | null;
    /**
     * Get a value based on hash from Visitor id to use for bucket selecting
     * @param {Id} visitorId
     * @param {number=} seed
     * @return {number}
     */
    getValueVisitorBased(visitorId: Id, seed?: number): number;
    /**
     * Get a bucket for the visitor
     * @param {object} buckets Key-value object with variations IDs as keys and percentages as values
     * @param {Id} visitorId
     * @param {number} [redistribute=0]
     * @param {number} [seed=]
     * @return {string | null}
     */
    getBucketForVisitor(buckets: Record<string, number>, visitorId: Id, redistribute?: number, seed?: number): string | null;
}
