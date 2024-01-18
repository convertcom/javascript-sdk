/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {Experience} from './Experience';
import {Feature} from './Feature';
import {Audience} from './Audience';
import {Location} from './Location';
import {Segments} from './Segments';
import {Goal} from './Goal';
import {Variation} from './Variation';

export type Entity =
  | Experience
  | Feature
  | Audience
  | Location
  | Segments
  | Goal
  | Variation;
