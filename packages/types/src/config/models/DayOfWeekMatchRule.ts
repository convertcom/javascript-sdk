/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithDayOfWeekValue } from './BaseRuleWithDayOfWeekValue';
import type { DayOfWeekMatchRulesTypes } from './DayOfWeekMatchRulesTypes';
import type { NumericMatchingOptions } from './NumericMatchingOptions';
export type DayOfWeekMatchRule = (BaseRuleWithDayOfWeekValue & {
    rule_type: DayOfWeekMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: NumericMatchingOptions;
    });
});

