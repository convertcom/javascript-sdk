/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {RuleSet} from './Rule';
import {Id} from './Id';

export type Location = {
  id: Id;
  name: string;
  key: string;
  rules: RuleSet;
};
