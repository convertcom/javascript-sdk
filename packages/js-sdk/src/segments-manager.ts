/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {SegmentsManagerInterface} from './interfaces/segments-manager';
import {Config, ConfigData} from './types/Config';
import {LogManagerInterface} from './interfaces/log-manager';
import {Id} from './types/Id';
import {objectDeepValue} from './utils/object-utils';
import {SegmentsData} from './types/SegmentsData';
import {DataManagerInterface} from './interfaces/data-manager';
import {SegmentsKeys} from './enums/segments/segments-keys';
import {StoreData} from './types/StoreData';
import {Segments} from './types/Segments';
import {MESSAGES} from './enums/dictionary';
import {RuleManagerInterface} from './interfaces/rule-manager';

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
    // Store the data in local variable
    this._dataManager.putLocalStore(visitorId, {segments});
    // Enqueue to store in dataStore
    const storeKey = this._dataManager.getStoreKey(visitorId);
    this._dataManager.dataStoreManager.enqueue(storeKey, {segments});
  }

  /**
   * Update custom segments for specific visitor
   * @param {Id} visitorId
   * @param {Array<string>} segmentKeys A list of segment keys
   * @param {Record<string, any>=} segmentRule An object of key-value pairs that are used for segments matching
   * @param {string=} environment
   * @return {SegmentsData}
   */
  selectCustomSegments(
    visitorId: Id,
    segmentKeys: Array<string>,
    segmentRule?: Record<string, any>
  ): SegmentsData {
    const segments = this._dataManager.getEntities(
      segmentKeys,
      'segments'
    ) as Array<Segments>;

    const storeData: StoreData =
      this._dataManager.getLocalStore(visitorId) || {};
    // Get custom segments ID from DataStore
    const {
      segments: {[SegmentsKeys.CUSTOM_SEGMENTS]: customSegments = []} = {}
    } = storeData;

    const segmentIds = [];

    let segmentsMatched = false;
    for (const segment of segments) {
      if (segmentRule && !segmentsMatched) {
        segmentsMatched = this._ruleManager.isRuleMatched(
          segmentRule,
          segment?.rules
        );
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
      this._loggerManager?.error?.(MESSAGES.SEGMENTS_NOT_FOUND);
    }

    return segmentsData;
  }
}
