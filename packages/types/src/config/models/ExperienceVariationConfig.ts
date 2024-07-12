/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { ExperienceChange } from './ExperienceChange';
import type { VariationStatuses } from './VariationStatuses';
/**
 * Variation Object
 */
export type ExperienceVariationConfig = {
    /**
     * Variation ID
     */
    id?: string;
    /**
     * Variation name
     */
    name?: string;
    /**
     * Variation Key
     */
    key?: string;
    /**
     * Percentage of traffic allocation for this variation, as a number from 0 to 10000.
     * For an experience, the sum of the traffic allocations for all variations cannot be greater than 10000.
     *
     */
    traffic_allocation?: number;
    status?: VariationStatuses;
    /**
     * List of changes that this variation is exposing.
     */
    changes?: Array<ExperienceChange>;
};

