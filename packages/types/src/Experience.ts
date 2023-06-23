/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {ExperienceStatus, ExperienceType} from '@convertcom/enums';
import {Id} from './Id';
import {RuleSet} from './Rule';
import {Variation} from './Variation';

export type Experience = {
  id: Id;
  name: string;
  key: string;
  site_area?: RuleSet;
  locations?: Array<Id>;
  audiences?: Array<Id>;
  goals?: Array<Id>;
  type: ExperienceType;
  version: number;
  url: string;
  global_js?: string;
  global_css?: string;
  variations: Array<Variation>;
  status: ExperienceStatus;
  environments: Array<string>;
  settings?: {
    min_order_value?: number;
    max_order_value?: number;
  };
};
