/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {Id} from './Id';
import {Visitor} from './tracking/Visitor';
export type TrackingEvent = {
  accountId?: Id;
  projectId?: Id;
  enrichData?: boolean;
  source?: string;
  visitors: Array<Visitor>;
};
