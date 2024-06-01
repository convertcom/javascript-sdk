/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {ConversionSettingKey, GoalDataKey} from '@convertcom/js-sdk-enums';

export type ConversionAttributes = {
  ruleData?: Record<string, any>;
  conversionData?: Array<Record<GoalDataKey, number>>;
  conversionSetting?: Record<ConversionSettingKey, number | string | boolean>;
};
