/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ExperienceChangeCustomCodeData } from './ExperienceChangeCustomCodeData';
import type { ExperienceChangeDefaultCodeData } from './ExperienceChangeDefaultCodeData';
import type { ExperienceChangeDefaultCodeMultipageData } from './ExperienceChangeDefaultCodeMultipageData';
import type { ExperienceChangeDefaultRedirectData } from './ExperienceChangeDefaultRedirectData';
import type { ExperienceChangeFullStackFeature } from './ExperienceChangeFullStackFeature';
import type { ExperienceChangeRichStructureData } from './ExperienceChangeRichStructureData';

/**
 * Object that represents one change done inside an experience
 */
export type ExperienceChange = (ExperienceChangeDefaultCodeData | ExperienceChangeDefaultCodeMultipageData | ExperienceChangeDefaultRedirectData | ExperienceChangeCustomCodeData | ExperienceChangeRichStructureData | ExperienceChangeFullStackFeature);