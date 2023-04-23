/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {
  arrayNotEmpty,
  Comparisons as DEFAULT_COMPARISON_PROCESSOR,
  objectDeepValue
} from '@convertcom/utils';

import {RuleManagerInterface} from './interfaces/rule-manager';

import {Config, Rule, RuleAnd, RuleOrWhen, RuleSet} from '@convertcom/types';
import {LogManagerInterface} from '@convertcom/logger';
import {ERROR_MESSAGES, MESSAGES} from '@convertcom/enums';

const DEFAULT_KEYS_CASE_SENSITIVE = true;
const DEFAULT_NEGATION = '!';

/**
 * Provides rule processing calculations with corresponding comparisons methods
 * @category Modules
 * @constructor
 * @implements {RuleManagerInterface}
 */
export class RuleManager implements RuleManagerInterface {
  private _comparisonProcessor: Record<string, any> =
    DEFAULT_COMPARISON_PROCESSOR;
  private readonly _negation: string = DEFAULT_NEGATION;
  private readonly _keys_case_sensitive: boolean = DEFAULT_KEYS_CASE_SENSITIVE;
  private _loggerManager: LogManagerInterface | null;
  /**
   * @param {Config=} config
   * @param {Object=} dependencies
   * @param {LogManagerInterface=} dependencies.loggerManager
   */
  constructor(
    config?: Config,
    {loggerManager}: {loggerManager?: LogManagerInterface} = {}
  ) {
    this._loggerManager = loggerManager;
    this._comparisonProcessor = objectDeepValue(
      config,
      'rules.comparisonProcessor',
      DEFAULT_COMPARISON_PROCESSOR
    );
    this._negation = String(
      objectDeepValue(config, 'rules.negation', DEFAULT_NEGATION)
    ).valueOf();

    this._keys_case_sensitive = objectDeepValue(
      config,
      'rules.keys_case_sensitive',
      DEFAULT_KEYS_CASE_SENSITIVE,
      true
    );
    this._loggerManager?.trace?.(MESSAGES.RULE_CONSTRUCTOR, this);
  }

  /**
   * Setter for comparison processor
   * @param {object} comparisonProcessor
   */
  set comparisonProcessor(comparisonProcessor: Record<string, any>) {
    this._comparisonProcessor = comparisonProcessor;
  }

  /**
   * Getter for comparison processor
   */
  get comparisonProcessor(): Record<string, any> {
    return this._comparisonProcessor;
  }

  /**
   * Retrieve comparison methods from comparison processor
   * @return {Array<string>} List of methods of comparison processor
   */
  getComparisonProcessorMethods(): Array<string> {
    return Object.getOwnPropertyNames(this._comparisonProcessor).filter(
      (name) => typeof this._comparisonProcessor[name] === 'function'
    );
  }

  /**
   * Check input data matching to rule set
   * @param {Record<string, string | number> | string | number} data Single value or key-value data set to compare
   * @param {RuleSet} ruleSet
   * @return {boolean}
   */
  isRuleMatched(
    data: Record<string, string | number> | string | number,
    ruleSet: RuleSet
  ): boolean {
    this._loggerManager?.debug?.('RuleManager.isRuleMatched()', {
      data: data,
      ruleSet: ruleSet
    });
    // Top OR level
    if (
      Object.prototype.hasOwnProperty.call(ruleSet, 'OR') &&
      arrayNotEmpty(ruleSet?.OR)
    ) {
      for (let i = 0, l = ruleSet.OR.length; i < l; i++) {
        if (this._processAND(data, ruleSet.OR[i])) {
          return true;
        }
      }
    } else {
      this._loggerManager?.warn?.(ERROR_MESSAGES.RULE_NOT_VALID);
    }
    return false;
  }

  /**
   * Check is rule object valid
   * @param {object} rule
   * @return {boolean}
   */
  isValidRule(rule: Rule): boolean {
    this._loggerManager?.debug?.('RuleManager.isValidRule()', {
      rule: rule
    });
    return (
      Object.prototype.hasOwnProperty.call(rule, 'key') && // comment when supporting direct value on _processRule()
      Object.prototype.hasOwnProperty.call(rule, 'matching') &&
      typeof rule.matching === 'object' &&
      Object.prototype.hasOwnProperty.call(rule.matching, 'match_type') &&
      typeof rule.matching.match_type === 'string' &&
      Object.prototype.hasOwnProperty.call(rule.matching, 'negated') &&
      typeof rule.matching.negated === 'boolean' &&
      Object.prototype.hasOwnProperty.call(rule, 'value')
    );
  }

  /**
   * Process AND block of rule set. Return first false if found
   * @param {object | string | number | boolean} data Single value or key-value data set to compare
   * @param {RuleAnd} rulesSubset
   * @return {boolean}
   * @private
   */
  private _processAND(
    data: Record<string, string | number> | string | number,
    rulesSubset: RuleAnd
  ): boolean {
    // Second AND level
    if (
      Object.prototype.hasOwnProperty.call(rulesSubset, 'AND') &&
      arrayNotEmpty(rulesSubset?.AND)
    ) {
      for (let i = 0, l = rulesSubset.AND.length; i < l; i++) {
        if (this._processORWHEN(data, rulesSubset.AND[i]) === false) {
          return false;
        }
      }
      return true;
    } else {
      this._loggerManager?.warn?.(ERROR_MESSAGES.RULE_NOT_VALID);
    }
    return false;
  }

  /**
   * Process OR block of rule set. Return first true if found
   * @param {object | string | number | boolean} data Single value or key-value data set to compare
   * @param {RuleOrWhen} rulesSubset
   * @return {boolean}
   * @private
   */
  private _processORWHEN(
    data: Record<string, string | number> | string | number,
    rulesSubset: RuleOrWhen
  ): boolean {
    // Third OR level. Called OR_WHEN.
    if (
      Object.prototype.hasOwnProperty.call(rulesSubset, 'OR_WHEN') &&
      arrayNotEmpty(rulesSubset?.OR_WHEN)
    ) {
      for (let i = 0, l = rulesSubset.OR_WHEN.length; i < l; i++) {
        if (this._processRule(data, rulesSubset.OR_WHEN[i])) {
          return true;
        }
      }
    } else {
      this._loggerManager?.warn?.(ERROR_MESSAGES.RULE_NOT_VALID);
    }
    return false;
  }

  /**
   * Process single rule
   * @param {object | string | number | boolean} data Single value or key-value data set to compare
   * @param {Rule} rule A single rule to compare
   * @return {boolean} Comparison result
   * @private
   */
  private _processRule(
    data: Record<string, string | number> | string | number,
    rule: Rule
  ): boolean {
    if (this.isValidRule(rule)) {
      try {
        const negation = rule.matching.negated || false;
        const matching = rule.matching.match_type;
        if (this.getComparisonProcessorMethods().indexOf(matching) !== -1) {
          const dataType = typeof data;
          switch (dataType) {
            // Validate direct value. Rule object `key` field is ignored or not present
            // case 'boolean':
            // case 'number':
            // case 'bigint':
            // case 'string':
            //   return this._comparisonProcessor[matching](
            //     data,
            //     rule.value,
            //     negation
            //   );
            //   break;
            // Validate data key-value set. Rule object has to have `key` field
            case 'object':
              for (const key of Object.keys(data)) {
                const k = this._keys_case_sensitive ? key : key.toLowerCase();
                const rule_k = this._keys_case_sensitive
                  ? rule.key
                  : rule.key.toLowerCase();
                if (k === rule_k) {
                  return this._comparisonProcessor[matching](
                    data[key],
                    rule.value,
                    negation
                  );
                }
              }
              break;
            default:
              this._loggerManager?.warn?.('RuleManager._processRule()', {
                warn: ERROR_MESSAGES.RULE_DATA_NOT_VALID
              });
          }
        }
      } catch (error) {
        this._loggerManager?.error?.('RuleManager._processRule()', {
          error: error.message
        });
      }
    } else {
      this._loggerManager?.warn?.(ERROR_MESSAGES.RULE_NOT_VALID);
    }
    return false;
  }
}
