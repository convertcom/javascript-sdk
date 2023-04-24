'use strict';

var utils = require('@convertcom/utils');
var enums = require('@convertcom/enums');

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
const DEFAULT_KEYS_CASE_SENSITIVE = true;
const DEFAULT_NEGATION = '!';
/**
 * Provides rule processing calculations with corresponding comparisons methods
 * @category Modules
 * @constructor
 * @implements {RuleManagerInterface}
 */
class RuleManager {
    /**
     * @param {Config=} config
     * @param {Object=} dependencies
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    constructor(config, { loggerManager } = {}) {
        var _a, _b;
        this._comparisonProcessor = utils.Comparisons;
        this._negation = DEFAULT_NEGATION;
        this._keys_case_sensitive = DEFAULT_KEYS_CASE_SENSITIVE;
        this._loggerManager = loggerManager;
        this._comparisonProcessor = utils.objectDeepValue(config, 'rules.comparisonProcessor', utils.Comparisons);
        this._negation = String(utils.objectDeepValue(config, 'rules.negation', DEFAULT_NEGATION)).valueOf();
        this._keys_case_sensitive = utils.objectDeepValue(config, 'rules.keys_case_sensitive', DEFAULT_KEYS_CASE_SENSITIVE, true);
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, enums.MESSAGES.RULE_CONSTRUCTOR, this);
    }
    /**
     * Setter for comparison processor
     * @param {object} comparisonProcessor
     */
    set comparisonProcessor(comparisonProcessor) {
        this._comparisonProcessor = comparisonProcessor;
    }
    /**
     * Getter for comparison processor
     */
    get comparisonProcessor() {
        return this._comparisonProcessor;
    }
    /**
     * Retrieve comparison methods from comparison processor
     * @return {Array<string>} List of methods of comparison processor
     */
    getComparisonProcessorMethods() {
        return Object.getOwnPropertyNames(this._comparisonProcessor).filter((name) => typeof this._comparisonProcessor[name] === 'function');
    }
    /**
     * Check input data matching to rule set
     * @param {Record<string, string | number> | string | number} data Single value or key-value data set to compare
     * @param {RuleSet} ruleSet
     * @return {boolean}
     */
    isRuleMatched(data, ruleSet) {
        var _a, _b, _c, _d;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.debug) === null || _b === void 0 ? void 0 : _b.call(_a, 'RuleManager.isRuleMatched()', {
            data: data,
            ruleSet: ruleSet
        });
        // Top OR level
        if (Object.prototype.hasOwnProperty.call(ruleSet, 'OR') &&
            utils.arrayNotEmpty(ruleSet === null || ruleSet === void 0 ? void 0 : ruleSet.OR)) {
            for (let i = 0, l = ruleSet.OR.length; i < l; i++) {
                if (this._processAND(data, ruleSet.OR[i])) {
                    return true;
                }
            }
        }
        else {
            (_d = (_c = this._loggerManager) === null || _c === void 0 ? void 0 : _c.warn) === null || _d === void 0 ? void 0 : _d.call(_c, enums.ERROR_MESSAGES.RULE_NOT_VALID);
        }
        return false;
    }
    /**
     * Check is rule object valid
     * @param {object} rule
     * @return {boolean}
     */
    isValidRule(rule) {
        var _a, _b;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.debug) === null || _b === void 0 ? void 0 : _b.call(_a, 'RuleManager.isValidRule()', {
            rule: rule
        });
        return (Object.prototype.hasOwnProperty.call(rule, 'key') && // comment when supporting direct value on _processRule()
            Object.prototype.hasOwnProperty.call(rule, 'matching') &&
            typeof rule.matching === 'object' &&
            Object.prototype.hasOwnProperty.call(rule.matching, 'match_type') &&
            typeof rule.matching.match_type === 'string' &&
            Object.prototype.hasOwnProperty.call(rule.matching, 'negated') &&
            typeof rule.matching.negated === 'boolean' &&
            Object.prototype.hasOwnProperty.call(rule, 'value'));
    }
    /**
     * Process AND block of rule set. Return first false if found
     * @param {object | string | number | boolean} data Single value or key-value data set to compare
     * @param {RuleAnd} rulesSubset
     * @return {boolean}
     * @private
     */
    _processAND(data, rulesSubset) {
        var _a, _b;
        // Second AND level
        if (Object.prototype.hasOwnProperty.call(rulesSubset, 'AND') &&
            utils.arrayNotEmpty(rulesSubset === null || rulesSubset === void 0 ? void 0 : rulesSubset.AND)) {
            for (let i = 0, l = rulesSubset.AND.length; i < l; i++) {
                if (this._processORWHEN(data, rulesSubset.AND[i]) === false) {
                    return false;
                }
            }
            return true;
        }
        else {
            (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.warn) === null || _b === void 0 ? void 0 : _b.call(_a, enums.ERROR_MESSAGES.RULE_NOT_VALID);
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
    _processORWHEN(data, rulesSubset) {
        var _a, _b;
        // Third OR level. Called OR_WHEN.
        if (Object.prototype.hasOwnProperty.call(rulesSubset, 'OR_WHEN') &&
            utils.arrayNotEmpty(rulesSubset === null || rulesSubset === void 0 ? void 0 : rulesSubset.OR_WHEN)) {
            for (let i = 0, l = rulesSubset.OR_WHEN.length; i < l; i++) {
                if (this._processRule(data, rulesSubset.OR_WHEN[i])) {
                    return true;
                }
            }
        }
        else {
            (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.warn) === null || _b === void 0 ? void 0 : _b.call(_a, enums.ERROR_MESSAGES.RULE_NOT_VALID);
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
    _processRule(data, rule) {
        var _a, _b, _c, _d, _e, _f;
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
                                    return this._comparisonProcessor[matching](data[key], rule.value, negation);
                                }
                            }
                            break;
                        default:
                            (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.warn) === null || _b === void 0 ? void 0 : _b.call(_a, 'RuleManager._processRule()', {
                                warn: enums.ERROR_MESSAGES.RULE_DATA_NOT_VALID
                            });
                    }
                }
            }
            catch (error) {
                (_d = (_c = this._loggerManager) === null || _c === void 0 ? void 0 : _c.error) === null || _d === void 0 ? void 0 : _d.call(_c, 'RuleManager._processRule()', {
                    error: error.message
                });
            }
        }
        else {
            (_f = (_e = this._loggerManager) === null || _e === void 0 ? void 0 : _e.warn) === null || _f === void 0 ? void 0 : _f.call(_e, enums.ERROR_MESSAGES.RULE_NOT_VALID);
        }
        return false;
    }
}

exports.RuleManager = RuleManager;
//# sourceMappingURL=index.js.map
