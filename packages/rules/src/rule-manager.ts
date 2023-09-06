/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {
  arrayNotEmpty,
  camelCase,
  Comparisons as DEFAULT_COMPARISON_PROCESSOR,
  objectDeepValue
} from '@convertcom/js-sdk-utils';

import {RuleManagerInterface} from './interfaces/rule-manager';

import {
  Config,
  Rule,
  RuleAnd,
  RuleOrWhen,
  RuleSet
} from '@convertcom/js-sdk-types';
import {LogManagerInterface} from '@convertcom/js-sdk-logger';
import {ERROR_MESSAGES, MESSAGES, RuleError} from '@convertcom/js-sdk-enums';

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
   * @param {Record<string, any>} comparisonProcessor
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
   * @param {Record<string, any>} data Single value or key-value data set to compare
   * @param {RuleSet} ruleSet
   * @return {boolean | RuleError}
   */
  isRuleMatched(
    data: Record<string, any>,
    ruleSet: RuleSet,
    logEntry?: string
  ): boolean | RuleError {
    this._loggerManager?.debug?.('RuleManager.isRuleMatched()', {
      data: data,
      ruleSet: ruleSet
    });
    if (logEntry) {
      this._loggerManager?.info?.(
        MESSAGES.PROCESSING_ENTITY.replace('#', logEntry)
      );
    }
    // Top OR level
    let match;
    if (
      Object.prototype.hasOwnProperty.call(ruleSet, 'OR') &&
      arrayNotEmpty(ruleSet?.OR)
    ) {
      for (let i = 0, l = ruleSet.OR.length; i < l; i++) {
        match = this._processAND(data, ruleSet.OR[i]);
        if (Object.values(RuleError).includes(match as RuleError)) {
          this._loggerManager?.info?.(
            logEntry || '',
            ERROR_MESSAGES.RULE_ERROR
          );
        } else {
          this._loggerManager?.info?.(
            logEntry || '',
            match === false
              ? MESSAGES.RULE_NOT_MATCH
              : MESSAGES.RULE_MATCH.replace('#', String(i))
          );
        }
        if (match !== false) {
          return match;
        }
      }
    } else {
      this._loggerManager?.warn?.(
        logEntry || '',
        ERROR_MESSAGES.RULE_NOT_VALID
      );
    }
    return false;
  }

  /**
   * Check is rule object valid
   * @param {Rule} rule
   * @return {boolean}
   */
  isValidRule(rule: Rule): boolean {
    this._loggerManager?.debug?.('RuleManager.isValidRule()', {
      rule: rule
    });
    return (
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
   * @param {Record<string, any>} data Single value or key-value data set to compare
   * @param {RuleAnd} rulesSubset
   * @return {boolean | RuleError}
   * @private
   */
  private _processAND(
    data: Record<string, any>,
    rulesSubset: RuleAnd
  ): boolean | RuleError {
    // Second AND level
    let match;
    if (
      Object.prototype.hasOwnProperty.call(rulesSubset, 'AND') &&
      arrayNotEmpty(rulesSubset?.AND)
    ) {
      for (let i = 0, l = rulesSubset.AND.length; i < l; i++) {
        match = this._processORWHEN(data, rulesSubset.AND[i]);
        if (match === false) {
          return false;
        }
      }
      if (match !== false) {
        this._loggerManager?.info?.(MESSAGES.RULE_MATCH_AND);
      }
      return match;
    } else {
      this._loggerManager?.warn?.(ERROR_MESSAGES.RULE_NOT_VALID);
    }
    return false;
  }

  /**
   * Process OR block of rule set. Return first true if found
   * @param {Record<string, any>} data Single value or key-value data set to compare
   * @param {RuleOrWhen} rulesSubset
   * @return {boolean | RuleError}
   * @private
   */
  private _processORWHEN(
    data: Record<string, any>,
    rulesSubset: RuleOrWhen
  ): boolean | RuleError {
    // Third OR level. Called OR_WHEN.
    let match;
    if (
      Object.prototype.hasOwnProperty.call(rulesSubset, 'OR_WHEN') &&
      arrayNotEmpty(rulesSubset?.OR_WHEN)
    ) {
      for (let i = 0, l = rulesSubset.OR_WHEN.length; i < l; i++) {
        match = this._processRuleItem(data, rulesSubset.OR_WHEN[i]);
        if (match !== false) {
          return match;
        }
      }
    } else {
      this._loggerManager?.warn?.(ERROR_MESSAGES.RULE_NOT_VALID);
    }
    return false;
  }

  /**
   * Process single rule item
   * @param {Record<string, any>} data Single value or key-value data set to compare
   * @param {Rule} rule A single rule to compare
   * @return {boolean | RuleError} Comparison result
   * @private
   */
  private _processRuleItem(
    data: Record<string, any>,
    rule: Rule
  ): boolean | RuleError {
    if (this.isValidRule(rule)) {
      try {
        const negation = rule.matching.negated || false;
        const matching = rule.matching.match_type;
        if (this.getComparisonProcessorMethods().indexOf(matching) !== -1) {
          if (typeof data === 'object') {
            // Validate data key-value set.
            if (data.constructor === Object) {
              // Rule object has to have `key` field
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
            } else if (rule?.rule_type) {
              // Rule object has to have `rule_type` field
              this._loggerManager?.info?.(
                MESSAGES.RULE_MATCH_START.replace('#', rule.rule_type)
              );
              for (const method of Object.getOwnPropertyNames(
                data.constructor.prototype
              )) {
                if (method === 'constructor') continue;
                const rule_method = camelCase(
                  `get ${rule.rule_type.replace(/_/g, ' ')}`
                );
                if (method === rule_method) {
                  const dataValue = data[method](rule);
                  if (Object.values(RuleError).includes(dataValue as RuleError))
                    return dataValue as RuleError;
                  if (rule.rule_type === 'js_condition') return dataValue;
                  return this._comparisonProcessor[matching](
                    dataValue,
                    rule.value,
                    negation
                  );
                }
              }
            }
          } else {
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
