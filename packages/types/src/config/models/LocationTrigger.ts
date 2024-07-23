/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { LocationTriggerCallback } from './LocationTriggerCallback';
import type { LocationTriggerDomChange } from './LocationTriggerDomChange';
import type { LocationTriggerManual } from './LocationTriggerManual';
import type { LocationTriggerUponRun } from './LocationTriggerUponRun';
/**
 * This one describes a logical triggering rule that is being used inside the app
 */
export type LocationTrigger = (LocationTriggerDomChange | LocationTriggerCallback | LocationTriggerManual | LocationTriggerUponRun);

