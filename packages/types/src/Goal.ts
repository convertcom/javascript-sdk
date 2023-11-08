/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {RuleSet} from './Rule';
import {Id} from './Id';
import {GoalType, GoalRevenueTriggeringType} from '@convertcom/js-sdk-enums';

export type GoalTrackedItem = {
  event: string;
  selector: string;
};

export type GoalSettings = {
  percentage?: number;
  tracked_items?: Array<GoalTrackedItem>;
  triggering_type?: GoalRevenueTriggeringType;
  selector?: string;
  href?: string;
  action?: string;
  ga_event?: string;
};

export type Goal = {
  id: Id;
  rules?: RuleSet;
  type: GoalType;
  settings?: GoalSettings;
  name: string;
  key: string;
};
