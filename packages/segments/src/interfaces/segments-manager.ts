/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {VisitorSegments} from '@convertcom/js-sdk-types';
import {RuleError} from '@convertcom/js-sdk-enums';

export interface SegmentsManagerInterface {
  getSegments(visitorId: string): VisitorSegments;
  putSegments(visitorId: string, segments: VisitorSegments): void;
  selectCustomSegments(
    visitorId: string,
    segmentKeys: Array<string>,
    segmentRule?: Record<string, any>
  ): VisitorSegments | RuleError;
  selectCustomSegmentsByIds(
    visitorId: string,
    segmentIds: Array<string>,
    segmentRule?: Record<string, any>
  ): VisitorSegments | RuleError;
}
