/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { LocationTriggerBase } from './LocationTriggerBase';

export type LocationTriggerUrlChange = (LocationTriggerBase & {
    type?: 'url_change';
});