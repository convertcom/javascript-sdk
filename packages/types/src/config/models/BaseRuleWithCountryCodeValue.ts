/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseRule } from './BaseRule';

export type BaseRuleWithCountryCodeValue = (BaseRule & {
    /**
     * The 2 letter ISO country code used for matching
     */
    value?: string;
});