/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { LocationTriggerBase } from './LocationTriggerBase';
export type LocationTriggerCallback = (LocationTriggerBase & {
    type?: LocationTriggerCallback.type;
    /**
     * Describes js callback to execute
     */
    js: string;
});
export namespace LocationTriggerCallback {
    export enum type {
        CALLBACK = 'callback',
    }
}

