/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {Id} from '../types/Id';
import {SegmentsData} from '../types/SegmentsData';

export interface SegmentsManagerInterface {
  getSegments(visitorId: Id): SegmentsData;
  putSegments(visitorId: Id, segments: SegmentsData): void;
  selectCustomSegments(
    visitorId: Id,
    segmentKeys: Array<string>,
    segmentRule?: Record<string, any>
  ): SegmentsData | null;
}
