/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BaseRule } from './BaseRule';

export type BaseRuleWithOsValue = (BaseRule & {
    /**
     * Operating System name used for matching
     */
    value?: 'android' | 'iphone' | 'ipod' | 'ipad' | 'windows' | 'macos' | 'linux';
});