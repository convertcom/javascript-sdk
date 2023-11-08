/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {VariationChangeType} from '@convertcom/js-sdk-enums';
import {Id} from './Id';

export type RichStructureChange = {
  js?: string;
  selector: string;
  page_id: string;
};

export type CustomCodeChange = {
  css?: string;
  js?: string;
  page_id: string;
};

export type DefaultCodeChange = {
  css?: string;
  js?: string;
  custom_js?: string;
};

export type DefaultCodeMultipageChange = {
  css?: string;
  js?: string;
  custom_js?: string;
  page_id: string;
};

export type DefaultRedirectChange = {
  original_pattern: string;
  variation_pattern: string;
};

export type FullStackFeatureChange = {
  feature_id: Id;
  variables_data: Record<string, any>;
};

export type VariationChange = {
  id: Id;
  type: VariationChangeType;
  data:
    | RichStructureChange
    | CustomCodeChange
    | DefaultCodeChange
    | DefaultCodeMultipageChange
    | DefaultRedirectChange
    | FullStackFeatureChange;
};

export type Variation = {
  id: Id;
  key: string;
  name: string;
  changes: Array<VariationChange>;
  traffic_allocation?: number;
};
