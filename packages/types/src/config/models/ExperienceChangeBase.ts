/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
/**
 * Object that represents the change done inside an experience
 */
export type ExperienceChangeBase = {
    type?: ExperienceChangeBase.type;
    /**
     * This contains all data of this change, any code, settings etc
     *
     * This is sent by default in the following requests responses: **getExperienceChange**;
     *
     * All other responses that return this field, will only return it if "include" request parameter contains its name
     *
     * Data object structure will correspond to the "type" field
     *
     */
    data?: Record<string, any>;
};
export namespace ExperienceChangeBase {
    export enum type {
        RICH_STRUCTURE = 'richStructure',
        CUSTOM_CODE = 'customCode',
        DEFAULT_CODE = 'defaultCode',
        DEFAULT_CODE_MULTIPAGE = 'defaultCodeMultipage',
        DEFAULT_REDIRECT = 'defaultRedirect',
        FULL_STACK_FEATURE = 'fullStackFeature',
    }
}

