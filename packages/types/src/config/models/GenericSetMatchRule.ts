/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithStringValue } from './BaseRuleWithStringValue';
import type { SetMatchingOptions } from './SetMatchingOptions';

export type GenericSetMatchRule = (BaseRuleWithStringValue & {
    rule_type: string;
    matching?: (BaseMatch & {
        match_type?: SetMatchingOptions;
    });
});