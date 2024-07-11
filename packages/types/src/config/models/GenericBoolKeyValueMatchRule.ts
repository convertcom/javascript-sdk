/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithBooleanValue } from './BaseRuleWithBooleanValue';
import type { ChoiceMatchingOptions } from './ChoiceMatchingOptions';
import type { GenericBoolKeyValueMatchRulesTypes } from './GenericBoolKeyValueMatchRulesTypes';
import type { GenericKey } from './GenericKey';

export type GenericBoolKeyValueMatchRule = (BaseRuleWithBooleanValue & {
    rule_type: GenericBoolKeyValueMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: ChoiceMatchingOptions;
    });
} & GenericKey);