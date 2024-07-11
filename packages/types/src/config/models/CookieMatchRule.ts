/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithStringValue } from './BaseRuleWithStringValue';
import type { CookieMatchRulesTypes } from './CookieMatchRulesTypes';
import type { TextMatchingOptions } from './TextMatchingOptions';

export type CookieMatchRule = (BaseRuleWithStringValue & {
    rule_type: CookieMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: TextMatchingOptions;
    });
    /**
     * The name of the cookie which value is compared to the given rule value
     */
    key?: string;
});