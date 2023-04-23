/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {Variation, Id, Experience, BucketedVariation} from '@convertcom/types';

export interface ExperienceManagerInterface {
  getList(): Array<Experience>;

  getExperience(key: string): Experience;
  getExperienceById(id: Id): Experience;
  getExperiences(keys: Array<string>): Array<Experience>;

  selectVariation(
    visitorId: Id,
    experienceKey: string,
    visitorProperties: Record<string, any>,
    locationProperties: Record<string, any>,
    environment?: string
  ): BucketedVariation | null;

  selectVariationById(
    visitorId: Id,
    experienceId: Id,
    visitorProperties: Record<string, any>,
    locationProperties: Record<string, any>,
    environment?: string
  ): BucketedVariation | null;

  selectVariations(
    visitorId: Id,
    visitorProperties: Record<string, any>,
    locationProperties: Record<string, any>,
    environment?: string
  ): Array<BucketedVariation>;

  getVariation(experienceKey: string, variationKey: string): Variation;

  getVariationById(experienceId: Id, variationId: Id): Variation;
}
