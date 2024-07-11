/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseRule } from './BaseRule';

export type BaseRuleWithHourOfDayValue = (BaseRule & {
    /**
     * Hour of day used for matching
     */
    value?: number;
});