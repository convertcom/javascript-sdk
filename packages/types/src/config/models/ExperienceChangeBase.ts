/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

/**
 * Object that represents the change done inside an experience
 */
export type ExperienceChangeBase = {
    type?: 'richStructure' | 'customCode' | 'defaultCode' | 'defaultCodeMultipage' | 'defaultRedirect' | 'fullStackFeature';
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
    data?: any;
}