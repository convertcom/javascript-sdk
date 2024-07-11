/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { LocationTriggerBase } from './LocationTriggerBase';
export type LocationTriggerDomChange = (LocationTriggerBase & {
    type?: LocationTriggerDomChange.type;
    /**
     * Describes html selector
     */
    selector: string;
    /**
     * Describes event
     */
    event: string;
});
export namespace LocationTriggerDomChange {
    export enum type {
        DOM_CHANGE = 'dom_change',
    }
}

