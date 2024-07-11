/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { LocationTriggerCallback } from './LocationTriggerCallback';
import type { LocationTriggerDomChange } from './LocationTriggerDomChange';
import type { LocationTriggerManual } from './LocationTriggerManual';
import type { LocationTriggerUrlChange } from './LocationTriggerUrlChange';

export type LocationTrigger = (LocationTriggerDomChange | LocationTriggerCallback | LocationTriggerManual | LocationTriggerUrlChange);