/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {SegmentsManagerInterface} from './interfaces/segments-manager';
import {
  Config,
  ConfigData,
  Id,
  Segments,
  SegmentsData,
  StoreData
} from '@convertcom/js-sdk-types';
import {MESSAGES, SegmentsKeys, RuleError} from '@convertcom/js-sdk-enums';
import {LogManagerInterface} from '@convertcom/js-sdk-logger';
import {objectDeepValue} from '@convertcom/js-sdk-utils';
import {DataManagerInterface} from '@convertcom/js-sdk-data';
import {RuleManagerInterface} from '@convertcom/js-sdk-rules';

/**
 * Provides segments specific logic
 * @category Modules
 * @constructor
 * @implements {SegmentsManagerInterface}
 */
export class SegmentsManager implements SegmentsManagerInterface {
  private _data: ConfigData;

  private _dataManager: DataManagerInterface;
  private _ruleManager: RuleManagerInterface;
  private _loggerManager: LogManagerInterface | null;

  /**
   * @param {Config} config
   * @param {Object} dependencies
   * @param {DataManagerInterface=} dependencies.dataManager
   * @param {RuleManagerInterface=} dependencies.ruleManager
   * @param {LogManagerInterface=} dependencies.loggerManager
   */
  constructor(
    config: Config,
    {
      dataManager,
      ruleManager,
      loggerManager
    }: {
      dataManager: DataManagerInterface;
      ruleManager: RuleManagerInterface;
      loggerManager?: LogManagerInterface;
    }
  ) {
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
  getSegments(visitorId: Id): SegmentsData {
    const storeData: StoreData =
      this._dataManager.getLocalStore(visitorId) || {};
    return storeData?.segments;
  }

  /**
   * Update segments in DataStore
   * @param {Id} visitorId
   * @param {SegmentsData} segments
   */
  putSegments(visitorId: Id, segments: SegmentsData): void {
    const storeData: StoreData =
      this._dataManager.getLocalStore(visitorId) || {};
    // Store the data in local variable
    this._dataManager.putLocalStore(visitorId, {...storeData, segments});
    // Enqueue to store in dataStore
    const storeKey = this._dataManager.getStoreKey(visitorId);
    this._dataManager.dataStoreManager.enqueue(storeKey, {
      ...storeData,
      segments
    });
  }

  private setCustomSegments(
    visitorId: Id,
    segments: Array<Segments>,
    segmentRule?: Record<string, any>
  ): SegmentsData | RuleError {
    const storeData: StoreData =
      this._dataManager.getLocalStore(visitorId) || {};
    // Get custom segments ID from DataStore
    const {
      segments: {[SegmentsKeys.CUSTOM_SEGMENTS]: customSegments = []} = {}
    } = storeData;

    const segmentIds = [];

    let segmentsMatched: boolean | RuleError = false;
    for (const segment of segments) {
      if (segmentRule && !segmentsMatched) {
        segmentsMatched = this._ruleManager.isRuleMatched(
          segmentRule,
          segment?.rules,
          `Segments #${segment?.id}`
        );
        // Return rule errors if present
        if (Object.values(RuleError).includes(segmentsMatched as RuleError))
          return segmentsMatched as RuleError;
      }

      if (!segmentRule || segmentsMatched) {
        const segmentId = segment?.id?.toString();
        if (customSegments.includes(segmentId)) {
          this._loggerManager?.warn?.(MESSAGES.CUSTOM_SEGMENTS_KEY_FOUND);
        } else {
          segmentIds.push(segmentId);
        }
      }
    }

    let segmentsData: SegmentsData;
    if (segmentIds.length) {
      segmentsData = {
        ...(storeData.segments || {}),
        [SegmentsKeys.CUSTOM_SEGMENTS]: [...customSegments, ...segmentIds]
      };

      // Merge custom segments ID into DataStore
      this.putSegments(visitorId, segmentsData);
    } else {
      this._loggerManager?.warn?.(MESSAGES.SEGMENTS_NOT_FOUND);
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
  selectCustomSegments(
    visitorId: Id,
    segmentKeys: Array<string>,
    segmentRule?: Record<string, any>
  ): SegmentsData | RuleError {
    const segments = this._dataManager.getEntities(
      segmentKeys,
      'segments'
    ) as Array<Segments>;

    return this.setCustomSegments(visitorId, segments, segmentRule);
  }

  /**
   * Update custom segments for specific visitor
   * @param {Id} visitorId
   * @param {Array<Id>} segmentIds A list of segment ids
   * @param {Record<string, any>=} segmentRule An object of key-value pairs that are used for segments matching
   * @param {string=} environment
   * @return {SegmentsData | RuleError}
   */
  selectCustomSegmentsByIds(
    visitorId: Id,
    segmentIds: Array<Id>,
    segmentRule?: Record<string, any>
  ): SegmentsData | RuleError {
    const segments = this._dataManager.getEntitiesByIds(
      segmentIds,
      'segments'
    ) as Array<Segments>;

    return this.setCustomSegments(visitorId, segments, segmentRule);
  }
}
