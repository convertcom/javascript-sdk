/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithLanguageCodeValue } from './BaseRuleWithLanguageCodeValue';
import type { ChoiceMatchingOptions } from './ChoiceMatchingOptions';
import type { LanguageMatchRulesTypes } from './LanguageMatchRulesTypes';

export type LanguageMatchRule = (BaseRuleWithLanguageCodeValue & {
    rule_type: LanguageMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: ChoiceMatchingOptions;
    });
});