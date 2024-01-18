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
  BucketedVariation
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
    visitorProperties: Record<string, any> | null,
    locationProperties: Record<string, any> | null,
    updateVisitorProperties?: boolean,
    environment?: string
  ): BucketedVariation | RuleError;

  selectVariationById(
    visitorId: Id,
    experienceId: Id,
    visitorProperties: Record<string, any> | null,
    locationProperties: Record<string, any> | null,
    updateVisitorProperties?: boolean,
    environment?: string
  ): BucketedVariation | RuleError;

  selectVariations(
    visitorId: Id,
    visitorProperties: Record<string, any> | null,
    locationProperties: Record<string, any> | null,
    updateVisitorProperties?: boolean,
    environment?: string
  ): Array<BucketedVariation | RuleError>;

  getVariation(experienceKey: string, variationKey: string): Variation;

  getVariationById(experienceId: Id, variationId: Id): Variation;
}
