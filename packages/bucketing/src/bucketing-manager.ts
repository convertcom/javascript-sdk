/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {BucketingManagerInterface} from './interfaces/bucketing-manager';

import {
  BucketingAllocation,
  BucketingHash,
  Config
} from '@convertcom/js-sdk-types';
import {generateHash} from '@convertcom/js-sdk-utils';
import {LogManagerInterface} from '@convertcom/js-sdk-logger';
import {MESSAGES} from '@convertcom/js-sdk-enums';

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
export class BucketingManager implements BucketingManagerInterface {
  private readonly _max_traffic: number = DEFAULT_MAX_TRAFFIC;
  private readonly _hash_seed: number = DEFAULT_HASH_SEED;
  private _loggerManager: LogManagerInterface | null;
  /**
   * @param {Config=} config
   * @param {Object=} dependencies
   * @param {LogManagerInterface=} dependencies.loggerManager
   */
  constructor(
    config?: Config,
    {loggerManager}: {loggerManager?: LogManagerInterface} = {}
  ) {
    this._loggerManager = loggerManager;
    this._max_traffic = config?.bucketing?.max_traffic || DEFAULT_MAX_TRAFFIC;
    this._hash_seed = config?.bucketing?.hash_seed || DEFAULT_HASH_SEED;
    this._loggerManager?.trace?.(
      'BucketingManager()',
      MESSAGES.BUCKETING_CONSTRUCTOR,
      this
    );
  }

  /**
   * Select variation based on its percentages and value provided
   * @param {object} buckets Key-value object with variations IDs as keys and percentages as values
   * @param {number} value A bucket value
   * @param {number=} redistribute Defaults to '0'
   * @return {string | null}
   */
  selectBucket(
    buckets: Record<string, number>,
    value: number,
    redistribute = 0
  ): string | null {
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
    this._loggerManager?.debug?.(
      'BucketingManager.selectBucket()',
      {
        buckets: buckets,
        value: value,
        redistribute: redistribute
      },
      {variation: variation}
    );
    return variation || null;
  }

  /**
   * Get a value based on hash from Visitor id to use for bucket selecting
   * @param {string} visitorId
   * @param {BucketingHash=} options
   * @param {number=} [options.seed=]
   * @param {string=} [options.experienceId=]
   * @return {number}
   */
  getValueVisitorBased(visitorId: string, options?: BucketingHash): number {
    const {seed = this._hash_seed, experienceId = ''} = options || {};
    const hash = generateHash(experienceId + String(visitorId), seed);
    const val = (hash / DEFAULT_MAX_HASH) * this._max_traffic;
    const result = parseInt(String(val), 10);
    this._loggerManager?.debug?.('BucketingManager.getValueVisitorBased()', {
      visitorId: visitorId,
      seed: seed,
      experienceId: experienceId,
      val: val,
      result: result
    });
    return result;
  }

  /**
   * Get a bucket for the visitor
   * @param {object} buckets Key-value object with variations IDs as keys and percentages as values
   * @param {string} visitorId
   * @param {BucketingHash=} options
   * @param {number=} [options.redistribute=0]
   * @param {number=} [options.seed=]
   * @param {string=} [options.experienceId=]
   * @return {BucketingAllocation | null}
   */
  getBucketForVisitor(
    buckets: Record<string, number>,
    visitorId: string,
    options?: BucketingHash
  ): BucketingAllocation | null {
    const value = this.getValueVisitorBased(visitorId, options);
    const selectedBucket = this.selectBucket(
      buckets,
      value,
      options?.redistribute
    );
    if (!selectedBucket) return null;
    return {
      variationId: selectedBucket,
      bucketingAllocation: value
    } as BucketingAllocation;
  }
}
