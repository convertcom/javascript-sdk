/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseRule } from './BaseRule';

export type BaseRuleWithGoalTriggeredValue = (BaseRule & {
    /**
     * ID of the goal used for matching
     */
    value?: number;
});