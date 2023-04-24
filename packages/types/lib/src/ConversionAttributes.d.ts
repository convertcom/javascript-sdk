/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import { GoalDataKey } from '@convertcom/enums';
export type ConversionAttributes = {
    ruleData?: Record<string, any>;
    conversionData?: Array<Record<GoalDataKey, number>>;
};
