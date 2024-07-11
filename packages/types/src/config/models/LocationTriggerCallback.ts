/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { LocationTriggerBase } from './LocationTriggerBase';

export type LocationTriggerCallback = (LocationTriggerBase & {
    type?: 'callback';
    /**
     * Describes js callback to execute
     */
    js: string;
});