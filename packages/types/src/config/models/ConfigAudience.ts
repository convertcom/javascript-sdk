/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { RuleObject } from './RuleObject';
/**
 * Base Audience object
 */
export type ConfigAudience = {
    /**
     * Audience ID
     */
    id?: string;
    /**
     * Audience unique key
     */
    key?: string;
    /**
     * Audience Name
     */
    name?: string;
    rules?: RuleObject | null;
};

