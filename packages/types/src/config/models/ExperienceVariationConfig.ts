/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ExperienceChange } from './ExperienceChange';

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
    /**
     * List of changes that this variation is exposing.
     */
    changes?: Array<ExperienceChange>;
}