/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {SegmentsManagerInterface} from './interfaces/segments-manager';
import {
  Config,
  ConfigResponseData,
  ConfigSegment,
  VisitorSegments,
  StoreData,
  RuleObject
} from '@convertcom/js-sdk-types';
import {MESSAGES, SegmentsKeys, RuleError} from '@convertcom/js-sdk-enums';
import {LogManagerInterface} from '@convertcom/js-sdk-logger';
import {DataManagerInterface} from '@convertcom/js-sdk-data';
import {RuleManagerInterface} from '@convertcom/js-sdk-rules';
import {objectDeepValue} from '@convertcom/js-sdk-utils';

/**
 * Provides segments specific logic
 * @category Modules
 * @constructor
 * @implements {SegmentsManagerInterface}
 */
export class SegmentsManager implements SegmentsManagerInterface {
  private _data: ConfigResponseData;

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
   * @param {string} visitorId
   * @returns {VisitorSegments}
   */
  getSegments(visitorId: string): VisitorSegments {
    const storeData: StoreData = this._dataManager.getData(visitorId) || {};
    const {segments} = this._dataManager.filterReportSegments(
      storeData?.segments
    );
    return segments;
  }

  /**
   * Update segments in DataStore
   * @param {string} visitorId
   * @param {VisitorSegments} segments
   */
  putSegments(visitorId: string, segments: VisitorSegments): void {
    const {segments: reportSegments} =
      this._dataManager.filterReportSegments(segments);
    if (reportSegments) {
      // Store the data in local variable
      this._dataManager.putData(visitorId, {segments: reportSegments});
    }
  }

  private setCustomSegments(
    visitorId: string,
    segments: Array<ConfigSegment>,
    segmentRule?: Record<string, any>
  ): VisitorSegments | RuleError {
    const storeKey = this._dataManager.getStoreKey(visitorId);
    let storeData: StoreData = this._dataManager.getData(visitorId) || {};
    // Get custom segments ID from DataStore
    let customSegments = [];
    const {
      segments: {[SegmentsKeys.CUSTOM_SEGMENTS]: localCustomSegments} = {}
    } = storeData;
    if (Array.isArray(localCustomSegments)) {
      customSegments = localCustomSegments.slice();
    } else {
      // Try to find a custom segments in dataStore
      storeData = this._dataManager?.dataStoreManager?.get?.(storeKey) || {};
      const {
        segments: {[SegmentsKeys.CUSTOM_SEGMENTS]: storedCustomSegments} = {}
      } = storeData;
      if (Array.isArray(storedCustomSegments)) {
        customSegments = storedCustomSegments.slice();
      }
    }

    const segmentIds = [];

    let segmentsMatched: boolean | RuleError = false;
    for (const segment of segments) {
      if (segmentRule && !segmentsMatched) {
        segmentsMatched = this._ruleManager.isRuleMatched(
          segmentRule,
          segment?.rules,
          `ConfigSegment #${segment?.id}`
        );
        // Return rule errors if present
        if (Object.values(RuleError).includes(segmentsMatched as RuleError))
          return segmentsMatched as RuleError;
      }

      if (!segmentRule || segmentsMatched) {
        const segmentId = segment?.id?.toString();
        if (customSegments.includes(segmentId)) {
          this._loggerManager?.warn?.(
            'SegmentsManager.setCustomSegments()',
            MESSAGES.CUSTOM_SEGMENTS_KEY_FOUND
          );
        } else {
          segmentIds.push(segmentId);
        }
      }
    }

    let segmentsData: VisitorSegments;
    if (segmentIds.length) {
      segmentsData = {
        ...(storeData.segments || {}),
        [SegmentsKeys.CUSTOM_SEGMENTS]: [...customSegments, ...segmentIds]
      };

      // Merge custom segments ID into DataStore
      this.putSegments(visitorId, segmentsData);
    } else {
      this._loggerManager?.warn?.(
        'SegmentsManager.setCustomSegments()',
        MESSAGES.SEGMENTS_NOT_FOUND
      );
    }

    return segmentsData;
  }

  /**
   * Update custom segments for specific visitor
   * @param {string} visitorId
   * @param {Array<string>} segmentKeys A list of segment keys
   * @param {Record<string, any>=} segmentRule An object of key-value pairs that are used for segments matching
   * @param {string=} environment
   * @return {VisitorSegments | RuleError}
   */
  selectCustomSegments(
    visitorId: string,
    segmentKeys: Array<string>,
    segmentRule?: Record<string, any>
  ): VisitorSegments | RuleError {
    const segments = this._dataManager.getEntities(
      segmentKeys,
      'segments'
    ) as Array<ConfigSegment>;

    return this.setCustomSegments(visitorId, segments, segmentRule);
  }

  /**
   * Update custom segments for specific visitor
   * @param {string} visitorId
   * @param {Array<string>} segmentIds A list of segment ids
   * @param {Record<string, any>=} segmentRule An object of key-value pairs that are used for segments matching
   * @param {string=} environment
   * @return {VisitorSegments | RuleError}
   */
  selectCustomSegmentsByIds(
    visitorId: string,
    segmentIds: Array<string>,
    segmentRule?: Record<string, any>
  ): VisitorSegments | RuleError {
    const segments = this._dataManager.getEntitiesByIds(
      segmentIds,
      'segments'
    ) as Array<ConfigSegment>;

    return this.setCustomSegments(visitorId, segments, segmentRule);
  }
}
