/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseRule } from './BaseRule';

export type BaseRuleWithBrowserNameValue = (BaseRule & {
    /**
     * Browser name used for matching
     */
    value?: 'chrome' | 'microsoft_ie' | 'firefox' | 'microsoft_edge' | 'mozilla' | 'opera' | 'safari';
});