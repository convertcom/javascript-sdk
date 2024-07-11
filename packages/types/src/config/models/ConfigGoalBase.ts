/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { GoalTypes } from './GoalTypes';
import type { RuleObject } from './RuleObject';

/**
 * Goal item to be tracked  inside a project
 */
export type ConfigGoalBase = {
    /**
     * Goal ID
     */
    id?: string;
    /**
     * Goal Name.
     */
    name?: string;
    /**
     * Goal Key
     */
    key?: string;
    /**
     * List of goal types to be returned
     */
    type?: Array<GoalTypes>;
    rules?: RuleObject | null;
}