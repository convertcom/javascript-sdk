/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {VisitorSegments} from './config/index';

export type StoreData = {
  bucketing?: Record<string, string>;
  locations?: Array<string>;
  segments?: VisitorSegments;
  goals?: Record<string, boolean>;
};
