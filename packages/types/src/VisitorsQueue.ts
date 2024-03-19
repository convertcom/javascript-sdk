/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {VisitorSegments, VisitorTrackingEvents} from './config/index';
import {Visitor} from './Visitor';
export type VisitorsQueue = {
  length: number;
  items: Array<Visitor>;
  push: (
    visitorId: string,
    eventRequest: VisitorTrackingEvents,
    segments?: VisitorSegments
  ) => void;
  reset: () => void;
};
