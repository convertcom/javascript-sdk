/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import { Id } from './Id';
import { Variation } from './Variation';
export type BucketedVariation = Variation & {
    experienceId?: Id;
    experienceKey?: string;
    experienceName?: string;
};
