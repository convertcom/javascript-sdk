/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { LocationTriggerBase } from './LocationTriggerBase';

export type LocationTriggerDomChange = (LocationTriggerBase & {
    type?: 'dom_change';
    /**
     * Describes html selector
     */
    selector: string;
    /**
     * Describes event
     */
    event: string;
});