/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { LocationTriggerBase } from './LocationTriggerBase';

export type LocationTriggerManual = (LocationTriggerBase & {
    type?: 'manual';
});