/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithSegmentBucketedValue } from './BaseRuleWithSegmentBucketedValue';
import type { ChoiceMatchingOptions } from './ChoiceMatchingOptions';
import type { SegmentBucketedMatchRulesTypes } from './SegmentBucketedMatchRulesTypes';

export type SegmentBucketedMatchRule = (BaseRuleWithSegmentBucketedValue & {
    rule_type: SegmentBucketedMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: ChoiceMatchingOptions;
    });
});