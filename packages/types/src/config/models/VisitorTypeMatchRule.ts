/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithVisitorTypeValue } from './BaseRuleWithVisitorTypeValue';
import type { ChoiceMatchingOptions } from './ChoiceMatchingOptions';
import type { VisitorTypeMatchRulesTypes } from './VisitorTypeMatchRulesTypes';
export type VisitorTypeMatchRule = (BaseRuleWithVisitorTypeValue & {
    rule_type: VisitorTypeMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: ChoiceMatchingOptions;
    });
});

