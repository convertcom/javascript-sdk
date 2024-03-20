/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithBooleanValue } from './BaseRuleWithBooleanValue';
import type { BoolMatchRulesTypes } from './BoolMatchRulesTypes';
import type { ChoiceMatchingOptions } from './ChoiceMatchingOptions';
export type GenericBoolMatchRule = (BaseRuleWithBooleanValue & {
    rule_type: BoolMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: ChoiceMatchingOptions;
    });
});

