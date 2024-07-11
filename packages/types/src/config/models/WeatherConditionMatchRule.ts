/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithWeatherConditionValue } from './BaseRuleWithWeatherConditionValue';
import type { TextMatchingOptions } from './TextMatchingOptions';
import type { WeatherConditionMatchRulesTypes } from './WeatherConditionMatchRulesTypes';

export type WeatherConditionMatchRule = (BaseRuleWithWeatherConditionValue & {
    rule_type: WeatherConditionMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: TextMatchingOptions;
    });
});