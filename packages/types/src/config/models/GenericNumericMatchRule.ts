/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithNumericValue } from './BaseRuleWithNumericValue';
import type { NumericMatchingOptions } from './NumericMatchingOptions';
import type { NumericMatchRulesTypes } from './NumericMatchRulesTypes';
export type GenericNumericMatchRule = (BaseRuleWithNumericValue & {
    rule_type: NumericMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: NumericMatchingOptions;
    });
});

