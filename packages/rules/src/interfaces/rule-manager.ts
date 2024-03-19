/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {RuleElement, RuleObject} from '@convertcom/js-sdk-types';
import {RuleError} from '@convertcom/js-sdk-enums';

export interface RuleManagerInterface {
  comparisonProcessor: Record<string, any>;

  getComparisonProcessorMethods(): Array<string>;

  isRuleMatched(
    data: Record<string, any>,
    ruleSet: RuleObject,
    logEntry?: string
  ): boolean | RuleError;

  isValidRule(rule: RuleElement): boolean;
}
