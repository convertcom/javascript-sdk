/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import { Id, SegmentsData } from '@convertcom/js-sdk-types';
import { RuleError } from '@convertcom/js-sdk-enums';
export interface SegmentsManagerInterface {
    getSegments(visitorId: Id): SegmentsData;
    putSegments(visitorId: Id, segments: SegmentsData): void;
    selectCustomSegments(visitorId: Id, segmentKeys: Array<string>, segmentRule?: Record<string, any>): SegmentsData | RuleError;
    selectCustomSegmentsByIds(visitorId: Id, segmentIds: Array<Id>, segmentRule?: Record<string, any>): SegmentsData | RuleError;
}
