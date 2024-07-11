/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseRule } from './BaseRule';

export type BaseRuleWithMinuteOfHourValue = (BaseRule & {
    /**
     * Minute of hour used for matching
     */
    value?: number;
});