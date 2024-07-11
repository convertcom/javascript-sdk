/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ExperienceChangeCustomCodeDataUpdate } from './ExperienceChangeCustomCodeDataUpdate';
import type { ExperienceChangeDefaultCodeDataUpdate } from './ExperienceChangeDefaultCodeDataUpdate';
import type { ExperienceChangeDefaultCodeMultipageDataUpdate } from './ExperienceChangeDefaultCodeMultipageDataUpdate';
import type { ExperienceChangeDefaultRedirectDataUpdate } from './ExperienceChangeDefaultRedirectDataUpdate';
import type { ExperienceChangeFullStackFeatureUpdate } from './ExperienceChangeFullStackFeatureUpdate';
import type { ExperienceChangeRichStructureDataUpdate } from './ExperienceChangeRichStructureDataUpdate';

/**
 * Object that represents one change done inside an experience
 */
export type ExperienceChangeUpdate = (ExperienceChangeDefaultCodeDataUpdate | ExperienceChangeDefaultCodeMultipageDataUpdate | ExperienceChangeDefaultRedirectDataUpdate | ExperienceChangeRichStructureDataUpdate | ExperienceChangeCustomCodeDataUpdate | ExperienceChangeFullStackFeatureUpdate);