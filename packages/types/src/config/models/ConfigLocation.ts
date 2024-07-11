/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { LocationTrigger } from './LocationTrigger';
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
    trigger?: (Record<string, any> & LocationTrigger) | null;
    rules?: RuleObject | null;
};

