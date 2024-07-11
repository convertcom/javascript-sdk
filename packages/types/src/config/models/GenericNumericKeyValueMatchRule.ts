/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithNumericValue } from './BaseRuleWithNumericValue';
import type { GenericKey } from './GenericKey';
import type { GenericNumericKeyValueMatchRulesTypes } from './GenericNumericKeyValueMatchRulesTypes';
import type { NumericMatchingOptions } from './NumericMatchingOptions';

export type GenericNumericKeyValueMatchRule = (BaseRuleWithNumericValue & {
    rule_type: GenericNumericKeyValueMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: NumericMatchingOptions;
    });
} & GenericKey);