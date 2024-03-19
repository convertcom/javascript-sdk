/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { RuleObject } from './RuleObject';
/**
 * Base Segment object
 */
export type ConfigSegment = {
    /**
     * Segment ID
     */
    id?: string;
    /**
     * Segment unique key
     */
    key?: string;
    /**
     * Segment Name
     */
    name?: string;
    rules?: RuleObject | null;
};

