/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {Visitor} from './Visitor';
export type TrackingEvent = {
  accountId?: string;
  projectId?: string;
  enrichData?: boolean;
  source?: string;
  visitors: Array<Visitor>;
};
