'use strict';

var utils = require('@convertcom/utils');
var enums = require('@convertcom/enums');

/******************************************************************************
Copyright (c) Microsoft Corporation.

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
***************************************************************************** */

function __values(o) {
    var s = typeof Symbol === "function" && Symbol.iterator, m = s && o[s], i = 0;
    if (m) return m.call(o);
    if (o && typeof o.length === "number") return {
        next: function () {
            if (o && i >= o.length) o = void 0;
            return { value: o && o[i++], done: !o };
        }
    };
    throw new TypeError(s ? "Object is not iterable." : "Symbol.iterator is not defined.");
}

var DEFAULT_KEYS_CASE_SENSITIVE = true;
var DEFAULT_NEGATION = '!';
/**
 * Provides rule processing calculations with corresponding comparisons methods
 * @category Modules
 * @constructor
 * @implements {RuleManagerInterface}
 */
var RuleManager = /** @class */ (function () {
    /**
     * @param {Config=} config
     * @param {Object=} dependencies
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    function RuleManager(config, _a) {
        var _b = _a === void 0 ? {} : _a, loggerManager = _b.loggerManager;
        var _c, _d;
        this._comparisonProcessor = utils.Comparisons;
        this._negation = DEFAULT_NEGATION;
        this._keys_case_sensitive = DEFAULT_KEYS_CASE_SENSITIVE;
        this._loggerManager = loggerManager;
        this._comparisonProcessor = utils.objectDeepValue(config, 'rules.comparisonProcessor', utils.Comparisons);
        this._negation = String(utils.objectDeepValue(config, 'rules.negation', DEFAULT_NEGATION)).valueOf();
        this._keys_case_sensitive = utils.objectDeepValue(config, 'rules.keys_case_sensitive', DEFAULT_KEYS_CASE_SENSITIVE, true);
        (_d = (_c = this._loggerManager) === null || _c === void 0 ? void 0 : _c.trace) === null || _d === void 0 ? void 0 : _d.call(_c, enums.MESSAGES.RULE_CONSTRUCTOR, this);
    }
    Object.defineProperty(RuleManager.prototype, "comparisonProcessor", {
        /**
         * Getter for comparison processor
         */
        get: function () {
            return this._comparisonProcessor;
        },
        /**
         * Setter for comparison processor
         * @param {object} comparisonProcessor
         */
        set: function (comparisonProcessor) {
            this._comparisonProcessor = comparisonProcessor;
        },
        enumerable: false,
        configurable: true
    });
    /**
     * Retrieve comparison methods from comparison processor
     * @return {Array<string>} List of methods of comparison processor
     */
    RuleManager.prototype.getComparisonProcessorMethods = function () {
        var _this = this;
        return Object.getOwnPropertyNames(this._comparisonProcessor).filter(function (name) { return typeof _this._comparisonProcessor[name] === 'function'; });
    };
    /**
     * Check input data matching to rule set
     * @param {Record<string, string | number> | string | number} data Single value or key-value data set to compare
     * @param {RuleSet} ruleSet
     * @return {boolean}
     */
    RuleManager.prototype.isRuleMatched = function (data, ruleSet) {
        var _a, _b, _c, _d;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.debug) === null || _b === void 0 ? void 0 : _b.call(_a, 'RuleManager.isRuleMatched()', {
            data: data,
            ruleSet: ruleSet
        });
        // Top OR level
        if (Object.prototype.hasOwnProperty.call(ruleSet, 'OR') &&
            utils.arrayNotEmpty(ruleSet === null || ruleSet === void 0 ? void 0 : ruleSet.OR)) {
            for (var i = 0, l = ruleSet.OR.length; i < l; i++) {
                if (this._processAND(data, ruleSet.OR[i])) {
                    return true;
                }
            }
        }
        else {
            (_d = (_c = this._loggerManager) === null || _c === void 0 ? void 0 : _c.warn) === null || _d === void 0 ? void 0 : _d.call(_c, enums.ERROR_MESSAGES.RULE_NOT_VALID);
        }
        return false;
    };
    /**
     * Check is rule object valid
     * @param {object} rule
     * @return {boolean}
     */
    RuleManager.prototype.isValidRule = function (rule) {
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
    };
    /**
     * Process AND block of rule set. Return first false if found
     * @param {object | string | number | boolean} data Single value or key-value data set to compare
     * @param {RuleAnd} rulesSubset
     * @return {boolean}
     * @private
     */
    RuleManager.prototype._processAND = function (data, rulesSubset) {
        var _a, _b;
        // Second AND level
        if (Object.prototype.hasOwnProperty.call(rulesSubset, 'AND') &&
            utils.arrayNotEmpty(rulesSubset === null || rulesSubset === void 0 ? void 0 : rulesSubset.AND)) {
            for (var i = 0, l = rulesSubset.AND.length; i < l; i++) {
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
    };
    /**
     * Process OR block of rule set. Return first true if found
     * @param {object | string | number | boolean} data Single value or key-value data set to compare
     * @param {RuleOrWhen} rulesSubset
     * @return {boolean}
     * @private
     */
    RuleManager.prototype._processORWHEN = function (data, rulesSubset) {
        var _a, _b;
        // Third OR level. Called OR_WHEN.
        if (Object.prototype.hasOwnProperty.call(rulesSubset, 'OR_WHEN') &&
            utils.arrayNotEmpty(rulesSubset === null || rulesSubset === void 0 ? void 0 : rulesSubset.OR_WHEN)) {
            for (var i = 0, l = rulesSubset.OR_WHEN.length; i < l; i++) {
                if (this._processRule(data, rulesSubset.OR_WHEN[i])) {
                    return true;
                }
            }
        }
        else {
            (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.warn) === null || _b === void 0 ? void 0 : _b.call(_a, enums.ERROR_MESSAGES.RULE_NOT_VALID);
        }
        return false;
    };
    /**
     * Process single rule
     * @param {object | string | number | boolean} data Single value or key-value data set to compare
     * @param {Rule} rule A single rule to compare
     * @return {boolean} Comparison result
     * @private
     */
    RuleManager.prototype._processRule = function (data, rule) {
        var e_1, _a;
        var _b, _c, _d, _e, _f, _g;
        if (this.isValidRule(rule)) {
            try {
                var negation = rule.matching.negated || false;
                var matching = rule.matching.match_type;
                if (this.getComparisonProcessorMethods().indexOf(matching) !== -1) {
                    var dataType = typeof data;
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
                            try {
                                for (var _h = __values(Object.keys(data)), _j = _h.next(); !_j.done; _j = _h.next()) {
                                    var key = _j.value;
                                    var k = this._keys_case_sensitive ? key : key.toLowerCase();
                                    var rule_k = this._keys_case_sensitive
                                        ? rule.key
                                        : rule.key.toLowerCase();
                                    if (k === rule_k) {
                                        return this._comparisonProcessor[matching](data[key], rule.value, negation);
                                    }
                                }
                            }
                            catch (e_1_1) { e_1 = { error: e_1_1 }; }
                            finally {
                                try {
                                    if (_j && !_j.done && (_a = _h.return)) _a.call(_h);
                                }
                                finally { if (e_1) throw e_1.error; }
                            }
                            break;
                        default:
                            (_c = (_b = this._loggerManager) === null || _b === void 0 ? void 0 : _b.warn) === null || _c === void 0 ? void 0 : _c.call(_b, 'RuleManager._processRule()', {
                                warn: enums.ERROR_MESSAGES.RULE_DATA_NOT_VALID
                            });
                    }
                }
            }
            catch (error) {
                (_e = (_d = this._loggerManager) === null || _d === void 0 ? void 0 : _d.error) === null || _e === void 0 ? void 0 : _e.call(_d, 'RuleManager._processRule()', {
                    error: error.message
                });
            }
        }
        else {
            (_g = (_f = this._loggerManager) === null || _f === void 0 ? void 0 : _f.warn) === null || _g === void 0 ? void 0 : _g.call(_f, enums.ERROR_MESSAGES.RULE_NOT_VALID);
        }
        return false;
    };
    return RuleManager;
}());

exports.RuleManager = RuleManager;
//# sourceMappingURL=index.js.map
