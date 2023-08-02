/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {GoalDataKey} from '@convertcom/js-sdk-enums';
import {Id} from '../Id';

export type ConversionEvent = {
  goalId: Id;
  goalData?: Array<Record<GoalDataKey, Id>>;
  bucketingData?: Record<string, Id>;
};
