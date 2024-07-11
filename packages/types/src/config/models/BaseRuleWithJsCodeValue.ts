/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseRule } from './BaseRule';

export type BaseRuleWithJsCodeValue = (BaseRule & {
    /**
     * The JS code that would be executed when rule is checked. The return value of this JS code is what is gonna be matched
     * against **true**(or **false** if **matching.negated = true** is provided)
     *
     */
    value?: string;
});