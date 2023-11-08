/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {Id} from './Id';
import {SegmentsData} from './SegmentsData';
import {Visitor} from './tracking/Visitor';
import {VisitorEvent} from './tracking/VisitorEvent';
export type VisitorsQueue = {
  length: number;
  items: Array<Visitor>;
  push: (
    visitorId: Id,
    eventRequest: VisitorEvent,
    segments?: SegmentsData
  ) => void;
  reset: () => void;
};
