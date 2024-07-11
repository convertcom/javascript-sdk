/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ExperienceChangeCustomCodeDataUpdateNoId } from './ExperienceChangeCustomCodeDataUpdateNoId';
import type { ExperienceChangeDefaultCodeDataUpdateNoId } from './ExperienceChangeDefaultCodeDataUpdateNoId';
import type { ExperienceChangeDefaultCodeMultipageDataUpdateNoId } from './ExperienceChangeDefaultCodeMultipageDataUpdateNoId';
import type { ExperienceChangeDefaultRedirectDataUpdateNoId } from './ExperienceChangeDefaultRedirectDataUpdateNoId';
import type { ExperienceChangeFullStackFeatureUpdateNoId } from './ExperienceChangeFullStackFeatureUpdateNoId';
import type { ExperienceChangeRichStructureDataUpdateNoId } from './ExperienceChangeRichStructureDataUpdateNoId';

/**
 * Object that represents one change done inside an experience
 */
export type ExperienceChangeUpdateNoId = (ExperienceChangeDefaultCodeDataUpdateNoId | ExperienceChangeDefaultCodeMultipageDataUpdateNoId | ExperienceChangeDefaultRedirectDataUpdateNoId | ExperienceChangeRichStructureDataUpdateNoId | ExperienceChangeCustomCodeDataUpdateNoId | ExperienceChangeFullStackFeatureUpdateNoId);