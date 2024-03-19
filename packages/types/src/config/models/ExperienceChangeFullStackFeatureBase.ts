/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { ExperienceChangeBase } from './ExperienceChangeBase';
/**
 * Describes structure for "fullStackFeature" type of experience change
 */
export type ExperienceChangeFullStackFeatureBase = (ExperienceChangeBase & {
    type?: ExperienceChangeFullStackFeatureBase.type;
    /**
     * Describes structure for "fullStackFeature" type of experience change
     */
    data?: {
        /**
         * The **id** of the feature connected to this change
         */
        feature_id?: number;
        /**
         * A key-value object defined by user which describes the variables values. Where the key is variable name defined in connected feature and value is a variable's value with corresponding type
         */
        variables_data?: Record<string, any>;
    };
});
export namespace ExperienceChangeFullStackFeatureBase {
    export enum type {
        FULL_STACK_FEATURE = 'fullStackFeature',
    }
}

