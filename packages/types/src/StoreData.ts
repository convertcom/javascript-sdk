/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {Id} from './Id';
import {SegmentsData} from './SegmentsData';

export type StoreData = {
  bucketing?: Record<string, Id>;
  locations?: Array<Id>;
  segments?: SegmentsData;
};
