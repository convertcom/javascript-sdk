/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ExperienceChangeCustomCodeDataAdd } from './ExperienceChangeCustomCodeDataAdd';
import type { ExperienceChangeDefaultCodeDataAdd } from './ExperienceChangeDefaultCodeDataAdd';
import type { ExperienceChangeDefaultCodeMultipageDataAdd } from './ExperienceChangeDefaultCodeMultipageDataAdd';
import type { ExperienceChangeDefaultRedirectDataAdd } from './ExperienceChangeDefaultRedirectDataAdd';
import type { ExperienceChangeFullStackFeatureAdd } from './ExperienceChangeFullStackFeatureAdd';
import type { ExperienceChangeRichStructureDataAdd } from './ExperienceChangeRichStructureDataAdd';

/**
 * Object that represents one change done inside an experience, used when adding changes
 */
export type ExperienceChangeAdd = (ExperienceChangeDefaultCodeDataAdd | ExperienceChangeDefaultCodeMultipageDataAdd | ExperienceChangeDefaultRedirectDataAdd | ExperienceChangeCustomCodeDataAdd | ExperienceChangeRichStructureDataAdd | ExperienceChangeFullStackFeatureAdd);