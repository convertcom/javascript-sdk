/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseRule } from './BaseRule';

export type BaseRuleWithStringValue = (BaseRule & {
    /**
     * The value used to match against 'rule_type' using 'matching'
     */
    value?: string;
});