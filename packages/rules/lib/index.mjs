import { Comparisons, objectDeepValue, arrayNotEmpty, camelCase } from '@convertcom/js-sdk-utils';
import { MESSAGES, ERROR_MESSAGES, RuleError } from '@convertcom/js-sdk-enums';

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
        this._comparisonProcessor = Comparisons;
        this._negation = DEFAULT_NEGATION;
        this._keys_case_sensitive = DEFAULT_KEYS_CASE_SENSITIVE;
        this._loggerManager = loggerManager;
        this._comparisonProcessor = objectDeepValue(config, 'rules.comparisonProcessor', Comparisons);
        this._negation = String(objectDeepValue(config, 'rules.negation', DEFAULT_NEGATION)).valueOf();
        this._keys_case_sensitive = objectDeepValue(config, 'rules.keys_case_sensitive', DEFAULT_KEYS_CASE_SENSITIVE, true);
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, MESSAGES.RULE_CONSTRUCTOR, this);
    }
    /**
     * Setter for comparison processor
     * @param {Record<string, any>} comparisonProcessor
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
     * @param {Record<string, any>} data Single value or key-value data set to compare
     * @param {RuleSet} ruleSet
     * @return {boolean | RuleError}
     */
    isRuleMatched(data, ruleSet) {
        var _a, _b, _c, _d;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.debug) === null || _b === void 0 ? void 0 : _b.call(_a, 'RuleManager.isRuleMatched()', {
            data: data,
            ruleSet: ruleSet
        });
        // Top OR level
        let match;
        if (Object.prototype.hasOwnProperty.call(ruleSet, 'OR') &&
            arrayNotEmpty(ruleSet === null || ruleSet === void 0 ? void 0 : ruleSet.OR)) {
            for (let i = 0, l = ruleSet.OR.length; i < l; i++) {
                match = this._processAND(data, ruleSet.OR[i]);
                if (match !== false) {
                    return match;
                }
            }
        }
        else {
            (_d = (_c = this._loggerManager) === null || _c === void 0 ? void 0 : _c.warn) === null || _d === void 0 ? void 0 : _d.call(_c, ERROR_MESSAGES.RULE_NOT_VALID);
        }
        return false;
    }
    /**
     * Check is rule object valid
     * @param {Rule} rule
     * @return {boolean}
     */
    isValidRule(rule) {
        var _a, _b;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.debug) === null || _b === void 0 ? void 0 : _b.call(_a, 'RuleManager.isValidRule()', {
            rule: rule
        });
        return (Object.prototype.hasOwnProperty.call(rule, 'matching') &&
            typeof rule.matching === 'object' &&
            Object.prototype.hasOwnProperty.call(rule.matching, 'match_type') &&
            typeof rule.matching.match_type === 'string' &&
            Object.prototype.hasOwnProperty.call(rule.matching, 'negated') &&
            typeof rule.matching.negated === 'boolean' &&
            Object.prototype.hasOwnProperty.call(rule, 'value'));
    }
    /**
     * Process AND block of rule set. Return first false if found
     * @param {Record<string, any>} data Single value or key-value data set to compare
     * @param {RuleAnd} rulesSubset
     * @return {boolean | RuleError}
     * @private
     */
    _processAND(data, rulesSubset) {
        var _a, _b;
        // Second AND level
        let match;
        if (Object.prototype.hasOwnProperty.call(rulesSubset, 'AND') &&
            arrayNotEmpty(rulesSubset === null || rulesSubset === void 0 ? void 0 : rulesSubset.AND)) {
            for (let i = 0, l = rulesSubset.AND.length; i < l; i++) {
                match = this._processORWHEN(data, rulesSubset.AND[i]);
                if (match === false) {
                    return false;
                }
            }
            return match;
        }
        else {
            (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.warn) === null || _b === void 0 ? void 0 : _b.call(_a, ERROR_MESSAGES.RULE_NOT_VALID);
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
    _processORWHEN(data, rulesSubset) {
        var _a, _b;
        // Third OR level. Called OR_WHEN.
        let match;
        if (Object.prototype.hasOwnProperty.call(rulesSubset, 'OR_WHEN') &&
            arrayNotEmpty(rulesSubset === null || rulesSubset === void 0 ? void 0 : rulesSubset.OR_WHEN)) {
            for (let i = 0, l = rulesSubset.OR_WHEN.length; i < l; i++) {
                match = this._processRuleItem(data, rulesSubset.OR_WHEN[i]);
                if (match !== false) {
                    return match;
                }
            }
        }
        else {
            (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.warn) === null || _b === void 0 ? void 0 : _b.call(_a, ERROR_MESSAGES.RULE_NOT_VALID);
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
    _processRuleItem(data, rule) {
        var _a, _b, _c, _d, _e, _f;
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
                                    return this._comparisonProcessor[matching](data[key], rule.value, negation);
                                }
                            }
                        }
                        else if (rule === null || rule === void 0 ? void 0 : rule.rule_type) {
                            // Rule object has to have `rule_type` field
                            for (const method of Object.getOwnPropertyNames(data.constructor.prototype)) {
                                if (method === 'constructor')
                                    continue;
                                const rule_method = camelCase(`get ${rule.rule_type.replace(/_/g, ' ')}`);
                                if (method === rule_method) {
                                    const dataValue = data[method](rule);
                                    if (Object.values(RuleError).includes(dataValue))
                                        return dataValue;
                                    if (rule.rule_type === 'js_condition')
                                        return dataValue;
                                    return this._comparisonProcessor[matching](dataValue, rule.value, negation);
                                }
                            }
                        }
                    }
                    else {
                        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.warn) === null || _b === void 0 ? void 0 : _b.call(_a, 'RuleManager._processRule()', {
                            warn: ERROR_MESSAGES.RULE_DATA_NOT_VALID
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
            (_f = (_e = this._loggerManager) === null || _e === void 0 ? void 0 : _e.warn) === null || _f === void 0 ? void 0 : _f.call(_e, ERROR_MESSAGES.RULE_NOT_VALID);
        }
        return false;
    }
}

export { RuleManager };
//# sourceMappingURL=index.mjs.map
