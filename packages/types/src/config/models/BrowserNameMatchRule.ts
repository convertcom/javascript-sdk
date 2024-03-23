/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithBrowserNameValue } from './BaseRuleWithBrowserNameValue';
import type { BrowserNameMatchRulesTypes } from './BrowserNameMatchRulesTypes';
import type { ChoiceMatchingOptions } from './ChoiceMatchingOptions';
export type BrowserNameMatchRule = (BaseRuleWithBrowserNameValue & {
    rule_type: BrowserNameMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: ChoiceMatchingOptions;
    });
});

