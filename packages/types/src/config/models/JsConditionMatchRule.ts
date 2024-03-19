/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithJsCodeValue } from './BaseRuleWithJsCodeValue';
import type { ChoiceMatchingOptions } from './ChoiceMatchingOptions';
import type { JsConditionMatchRulesTypes } from './JsConditionMatchRulesTypes';
export type JsConditionMatchRule = (BaseRuleWithJsCodeValue & {
    rule_type: JsConditionMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: ChoiceMatchingOptions;
    });
});

