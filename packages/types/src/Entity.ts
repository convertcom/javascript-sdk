/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {
  ConfigAudience,
  ConfigExperience,
  ConfigFeature,
  ConfigGoal,
  ConfigLocation,
  ConfigSegment,
  ExperienceVariationConfig
} from './config/index';

export type Entity =
  | ConfigExperience
  | ConfigFeature
  | ConfigAudience
  | ConfigLocation
  | ConfigSegment
  | ConfigGoal
  | ExperienceVariationConfig;
