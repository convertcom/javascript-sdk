/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {Rule, RuleSet} from '@convertcom/types';

export interface RuleManagerInterface {
  comparisonProcessor: Record<string, any>;

  getComparisonProcessorMethods(): Array<string>;

  isRuleMatched(data: Record<string, any>, ruleSet: RuleSet): boolean;

  isValidRule(rule: Rule): boolean;
}
