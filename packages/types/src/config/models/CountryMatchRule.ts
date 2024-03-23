/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithCountryCodeValue } from './BaseRuleWithCountryCodeValue';
import type { ChoiceMatchingOptions } from './ChoiceMatchingOptions';
import type { CountryMatchRulesTypes } from './CountryMatchRulesTypes';
export type CountryMatchRule = (BaseRuleWithCountryCodeValue & {
    rule_type: CountryMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: ChoiceMatchingOptions;
    });
});

