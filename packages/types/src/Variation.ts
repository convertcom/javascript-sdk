/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {Id} from './Id';

export type Variation = {
  id: Id;
  key: string;
  name: string;
  changes: Array<Record<string, any>>;
  traffic_allocation?: number;
};
