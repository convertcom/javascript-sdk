/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {
  Variation,
  Id,
  Experience,
  BucketedVariation,
  BucketingAttributes
} from '@convertcom/js-sdk-types';
import {RuleError} from '@convertcom/js-sdk-enums';

export interface ExperienceManagerInterface {
  getList(): Array<Experience>;

  getExperience(key: string): Experience;
  getExperienceById(id: Id): Experience;
  getExperiences(keys: Array<string>): Array<Experience>;

  selectVariation(
    visitorId: Id,
    experienceKey: string,
    attributes: BucketingAttributes
  ): BucketedVariation | RuleError;

  selectVariationById(
    visitorId: Id,
    experienceId: Id,
    attributes: BucketingAttributes
  ): BucketedVariation | RuleError;

  selectVariations(
    visitorId: Id,
    attributes: BucketingAttributes
  ): Array<BucketedVariation | RuleError>;

  getVariation(experienceKey: string, variationKey: string): Variation;

  getVariationById(experienceId: Id, variationId: Id): Variation;
}
