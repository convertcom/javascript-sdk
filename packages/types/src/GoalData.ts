/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {GoalDataKey} from '@convertcom/js-sdk-enums';

export type GoalData = {
  key: GoalDataKey;
  value: number | string;
};
