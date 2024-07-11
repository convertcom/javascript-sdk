/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ExperienceChangeBase } from './ExperienceChangeBase';

/**
 * Describes structure for "defaultCode" type of experience change
 */
export type ExperienceChangeCustomCodeDataBase = (ExperienceChangeBase & {
    type?: 'customCode';
    /**
     * Describes structure for "defaultCode" type of experience change
     */
    data?: {
        /**
         * CSS code to be applied by this change
         */
        css?: string | null;
        /**
         * Custom javascript code to be applied by this change
         */
        js?: string | null;
        /**
         * The **id** of the page connected to this change, in case this is a **multi-page** experiment
         */
        page_id?: string;
    };
});