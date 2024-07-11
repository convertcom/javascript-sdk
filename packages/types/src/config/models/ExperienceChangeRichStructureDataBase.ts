/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ExperienceChangeBase } from './ExperienceChangeBase';

/**
 * Describes structure for "defaultCode" type of experience change
 */
export type ExperienceChangeRichStructureDataBase = (ExperienceChangeBase & {
    type?: 'richStructure';
    /**
     * Describes structure for "defaultCode" type of experience change
     */
    data?: Record<string, string>;
});