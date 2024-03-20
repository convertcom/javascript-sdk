/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithMinuteOfHourValue } from './BaseRuleWithMinuteOfHourValue';
import type { MinuteOfHourMatchRulesTypes } from './MinuteOfHourMatchRulesTypes';
import type { NumericMatchingOptions } from './NumericMatchingOptions';
export type MinuteOfHourMatchRule = (BaseRuleWithMinuteOfHourValue & {
    rule_type: MinuteOfHourMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: NumericMatchingOptions;
    });
});

