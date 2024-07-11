/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseRule } from './BaseRule';

export type BaseRuleWithSegmentBucketedValue = (BaseRule & {
    /**
     * ID of the segment used for matching
     */
    value?: number;
});