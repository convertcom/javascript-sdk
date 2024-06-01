/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { ExperienceChangeBase } from './ExperienceChangeBase';
/**
 * Describes structure for "defaultRedirect" type of experience change
 */
export type ExperienceChangeDefaultRedirectDataBase = (ExperienceChangeBase & {
    type?: ExperienceChangeDefaultRedirectDataBase.type;
    /**
     * Describes structure for "defaultRedirect" type of experience change
     */
    data?: {
        /**
         * Defines whether the URL matching is case sensitive or not
         */
        case_sensitive?: boolean;
        /**
         * Pattern for matching the Original URL in order to construct the redirect URL
         */
        original_pattern?: string;
        /**
         * String used to construct the variation redirect URL. This string can contain matches from original_url or it can be a standard URL
         */
        variation_pattern?: string;
    };
});
export namespace ExperienceChangeDefaultRedirectDataBase {
    export enum type {
        DEFAULT_REDIRECT = 'defaultRedirect',
    }
}

