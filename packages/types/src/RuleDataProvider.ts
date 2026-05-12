/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {RuleElement} from './config/index';

/**
 * Optional custom rule-data provider passed to the SDK as
 * `Config.ruleDataProvider`. When set, DataManager hands this object to
 * RuleManager.isRuleMatched in place of the plain visitor/location
 * properties, enabling evaluation of web-specific rule types (URL,
 * cookie, geo, device, browser, etc.) that the tracking script's
 * `RuleData` class already implements.
 *
 * RuleManager dispatches on `rule_type` → camelCased `get<RuleType>()`
 * method. Implement only the getters for the rule types your config
 * actually uses; unimplemented getters cause the rule to evaluate as
 * `false` (no match).
 *
 * The `name: 'RuleData'` discriminator is mandatory — RuleManager uses
 * it to switch from plain-object key lookup to method-dispatch.
 */
export interface RuleDataProvider {
  /**
   * Discriminator required by `RuleManager.isUsingCustomInterface`.
   * Must be exactly the string `'RuleData'`.
   */
  readonly name: 'RuleData';

  /**
   * Optional method-name remapper. Some build pipelines mangle property
   * names; supply a mapper so RuleManager can still find the getter for
   * a given `rule_type`.
   */
  mapper?: (method: string) => string;

  /**
   * Index signature for the dynamic `get<RuleType>(rule)` getters.
   * Each getter receives the rule element and returns the visitor's
   * value for that rule type (or a RuleError sentinel when data is
   * missing).
   */
  [getter: string]: any;
}

/**
 * Common getter signatures consumers will likely implement. Listed here
 * for documentation / IntelliSense; not enforced by the index signature
 * above.
 */
export type RuleDataGetter<T = unknown> = (rule: RuleElement) => T;
