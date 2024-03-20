/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithOsValue } from './BaseRuleWithOsValue';
import type { ChoiceMatchingOptions } from './ChoiceMatchingOptions';
import type { OsMatchRulesTypes } from './OsMatchRulesTypes';
export type OsMatchRule = (BaseRuleWithOsValue & {
    rule_type: OsMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: ChoiceMatchingOptions;
    });
});

