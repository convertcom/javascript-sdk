/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {VariationType} from '@convertcom/js-sdk-enums';
import {Id} from './Id';

export type VariationChange = {
  id: Id;
  type: VariationType;
  data: {
    feature_id?: Id;
    variables_data?: Record<string, any>;
    [key: string]: Id | Record<string, any>;
  };
};

export type Variation = {
  id: Id;
  key: string;
  name: string;
  changes: Array<VariationChange>;
  traffic_allocation?: number;
};
