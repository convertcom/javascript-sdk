/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseRule } from './BaseRule';

export type BaseRuleWithVisitorTypeValue = (BaseRule & {
    /**
     * Type of the visitors
     */
    value?: 'new' | 'returning';
});