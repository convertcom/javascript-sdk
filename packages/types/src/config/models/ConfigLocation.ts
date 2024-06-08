/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { LocationTriggerTypes } from './LocationTriggerTypes';
import type { RuleObject } from './RuleObject';
/**
 * Base Location object
 */
export type ConfigLocation = {
    /**
     * Location ID
     */
    id?: string;
    /**
     * Location unique key
     */
    key?: string;
    /**
     * Location Name
     */
    name?: string;
    trigger?: LocationTriggerTypes;
    rules?: RuleObject | null;
};

