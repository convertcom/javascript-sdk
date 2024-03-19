/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithStringValue } from './BaseRuleWithStringValue';
import type { GenericKey } from './GenericKey';
import type { GenericTextKeyValueMatchRulesTypes } from './GenericTextKeyValueMatchRulesTypes';
import type { TextMatchingOptions } from './TextMatchingOptions';
export type GenericTextKeyValueMatchRule = (BaseRuleWithStringValue & {
    rule_type: GenericTextKeyValueMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: TextMatchingOptions;
    });
} & GenericKey);

