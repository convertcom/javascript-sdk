/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithStringValue } from './BaseRuleWithStringValue';
import type { TextMatchingOptions } from './TextMatchingOptions';
import type { TextMatchRulesTypes } from './TextMatchRulesTypes';

export type GenericTextMatchRule = (BaseRuleWithStringValue & {
    rule_type: TextMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: TextMatchingOptions;
    });
});