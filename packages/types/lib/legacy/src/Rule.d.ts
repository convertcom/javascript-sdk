/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
export type Rule = {
    rule_type?: string;
    key?: string;
    matching: {
        match_type: string;
        negated?: boolean;
    };
    value: string | number | boolean | ((...args: any) => void);
};
export type RuleOrWhen = {
    OR_WHEN: Array<Rule>;
};
export type RuleAnd = {
    AND: Array<RuleOrWhen>;
};
export type RuleSet = {
    OR: Array<RuleAnd>;
};
