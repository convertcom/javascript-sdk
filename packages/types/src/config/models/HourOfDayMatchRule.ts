/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithHourOfDayValue } from './BaseRuleWithHourOfDayValue';
import type { HourOfDayMatchRulesTypes } from './HourOfDayMatchRulesTypes';
import type { NumericMatchingOptions } from './NumericMatchingOptions';

export type HourOfDayMatchRule = (BaseRuleWithHourOfDayValue & {
    rule_type: HourOfDayMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: NumericMatchingOptions;
    });
});