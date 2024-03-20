/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { ExperienceChangeBase } from './ExperienceChangeBase';
/**
 * Describes structure for "defaultCode" type of experience change
 */
export type ExperienceChangeDefaultCodeDataBase = (ExperienceChangeBase & {
    type?: ExperienceChangeDefaultCodeDataBase.type;
    /**
     * Describes structure for "defaultCode" type of experience change
     */
    data?: {
        /**
         * CSS code to be applied by this change
         */
        css?: string | null;
        /**
         * Javascript code generated by the visual editor or written in the same structure, to be applied by this experience change
         */
        js?: string | null;
        /**
         * Custom javascript code to be applied by this change
         */
        custom_js?: string | null;
    };
});
export namespace ExperienceChangeDefaultCodeDataBase {
    export enum type {
        DEFAULT_CODE = 'defaultCode',
    }
}

