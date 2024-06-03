/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {ConversionSettingKey} from '@convertcom/js-sdk-enums';
import {GoalData} from './GoalData';

export type ConversionAttributes = {
  ruleData?: Record<string, any>;
  conversionData?: Array<GoalData>;
  conversionSetting?: Record<ConversionSettingKey, number | string | boolean>;
};
