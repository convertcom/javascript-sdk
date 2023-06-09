/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {Rule, RuleSet} from '../types/Rule';

export interface RuleManagerInterface {
  comparisonProcessor: Record<string, any>;

  getComparisonProcessorMethods(): Array<string>;

  isRuleMatched(
    data: Record<string, string | number> | string | number,
    ruleSet: RuleSet
  ): boolean;

  isValidRule(rule: Rule): boolean;
}
