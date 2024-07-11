/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseRule } from './BaseRule';

export type BaseRuleWithDayOfWeekValue = (BaseRule & {
    /**
     * Day of week used for matching
     */
    value?: number;
});