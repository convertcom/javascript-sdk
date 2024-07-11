/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
/**
 * This one describes a logical triggering rule that is being used inside the app
 */
export type LocationTriggerBase = {
    type: LocationTriggerBase.type;
};
export namespace LocationTriggerBase {
    export enum type {
        URL_CHANGE = 'url_change',
        MANUAL = 'manual',
        DOM_CHANGE = 'dom_change',
        CALLBACK = 'callback',
    }
}

