/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {VisitorSegments, VisitorTrackingEvents} from './config/index';
export type Visitor = {
  visitorId: string;
  segments?: VisitorSegments;
  events: Array<VisitorTrackingEvents>;
};
