/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {
  ExperienceIntegrationBaidu,
  ExperienceIntegrationClicktale,
  ExperienceIntegrationClicky,
  ExperienceIntegrationCnzz,
  ExperienceIntegrationCrazyegg,
  ExperienceIntegrationEconda,
  ExperienceIntegrationEulerian,
  ExperienceIntegrationGAServing,
  ExperienceIntegrationGosquared,
  ExperienceIntegrationHeapanalytics,
  ExperienceIntegrationHotjar,
  ExperienceIntegrationMixpanel,
  ExperienceIntegrationMouseflow,
  ExperienceIntegrationPiwik,
  ExperienceIntegrationSegmentio,
  ExperienceIntegrationSitecatalyst,
  ExperienceIntegrationWoopra,
  ExperienceIntegrationYsance
} from './config/index';

export type Integration =
  | ExperienceIntegrationBaidu
  | ExperienceIntegrationClicktale
  | ExperienceIntegrationClicky
  | ExperienceIntegrationCnzz
  | ExperienceIntegrationCrazyegg
  | ExperienceIntegrationEconda
  | ExperienceIntegrationEulerian
  | ExperienceIntegrationGAServing
  | ExperienceIntegrationGosquared
  | ExperienceIntegrationHeapanalytics
  | ExperienceIntegrationHotjar
  | ExperienceIntegrationMixpanel
  | ExperienceIntegrationMouseflow
  | ExperienceIntegrationPiwik
  | ExperienceIntegrationSegmentio
  | ExperienceIntegrationSitecatalyst
  | ExperienceIntegrationWoopra
  | ExperienceIntegrationYsance;
