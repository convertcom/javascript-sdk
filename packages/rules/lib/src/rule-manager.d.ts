import { RuleManagerInterface } from './interfaces/rule-manager';
import { Config, Rule, RuleSet } from '@convertcom/js-sdk-types';
import { LogManagerInterface } from '@convertcom/js-sdk-logger';
import { RuleError } from '@convertcom/js-sdk-enums';
/**
 * Provides rule processing calculations with corresponding comparisons methods
 * @category Modules
 * @constructor
 * @implements {RuleManagerInterface}
 */
export declare class RuleManager implements RuleManagerInterface {
    private _comparisonProcessor;
    private readonly _negation;
    private readonly _keys_case_sensitive;
    private _loggerManager;
    /**
     * @param {Config=} config
     * @param {Object=} dependencies
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    constructor(config?: Config, { loggerManager }?: {
        loggerManager?: LogManagerInterface;
    });
    /**
     * Setter for comparison processor
     * @param {Record<string, any>} comparisonProcessor
     */
    set comparisonProcessor(comparisonProcessor: Record<string, any>);
    /**
     * Getter for comparison processor
     */
    get comparisonProcessor(): Record<string, any>;
    /**
     * Retrieve comparison methods from comparison processor
     * @return {Array<string>} List of methods of comparison processor
     */
    getComparisonProcessorMethods(): Array<string>;
    /**
     * Check input data matching to rule set
     * @param {Record<string, any>} data Single value or key-value data set to compare
     * @param {RuleSet} ruleSet
     * @return {boolean | RuleError}
     */
    isRuleMatched(data: Record<string, any>, ruleSet: RuleSet): boolean | RuleError;
    /**
     * Check is rule object valid
     * @param {Rule} rule
     * @return {boolean}
     */
    isValidRule(rule: Rule): boolean;
    /**
     * Process AND block of rule set. Return first false if found
     * @param {Record<string, any>} data Single value or key-value data set to compare
     * @param {RuleAnd} rulesSubset
     * @return {boolean | RuleError}
     * @private
     */
    private _processAND;
    /**
     * Process OR block of rule set. Return first true if found
     * @param {Record<string, any>} data Single value or key-value data set to compare
     * @param {RuleOrWhen} rulesSubset
     * @return {boolean | RuleError}
     * @private
     */
    private _processORWHEN;
    /**
     * Process single rule item
     * @param {Record<string, any>} data Single value or key-value data set to compare
     * @param {Rule} rule A single rule to compare
     * @return {boolean | RuleError} Comparison result
     * @private
     */
    private _processRuleItem;
}
