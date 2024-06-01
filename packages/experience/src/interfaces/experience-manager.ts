/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {
  ExperienceVariationConfig,
  ConfigExperience,
  BucketedVariation,
  BucketingAttributes
} from '@convertcom/js-sdk-types';
import {BucketingError, RuleError} from '@convertcom/js-sdk-enums';

export interface ExperienceManagerInterface {
  getList(): Array<ConfigExperience>;

  getExperience(key: string): ConfigExperience;
  getExperienceById(id: string): ConfigExperience;
  getExperiences(keys: Array<string>): Array<ConfigExperience>;

  selectVariation(
    visitorId: string,
    experienceKey: string,
    attributes: BucketingAttributes
  ): BucketedVariation | RuleError | BucketingError;

  selectVariationById(
    visitorId: string,
    experienceId: string,
    attributes: BucketingAttributes
  ): BucketedVariation | RuleError | BucketingError;

  selectVariations(
    visitorId: string,
    attributes: BucketingAttributes
  ): Array<BucketedVariation | RuleError | BucketingError>;

  getVariation(
    experienceKey: string,
    variationKey: string
  ): ExperienceVariationConfig;

  getVariationById(
    experienceId: string,
    variationId: string
  ): ExperienceVariationConfig;
}
