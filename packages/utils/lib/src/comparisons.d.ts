/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
/**
 * Comparison Processor. Provides comparison methods for rules validation
 */
export declare class Comparisons {
    static equals(value: string | number | boolean | Array<string | number | boolean> | Record<string, string | number | boolean>, testAgainst: string | number | boolean, negation?: boolean): boolean;
    static equalsNumber: typeof Comparisons.equals;
    static matches: typeof Comparisons.equals;
    static less(value: string | number, testAgainst: string | number, negation?: boolean): boolean;
    static lessEqual(value: string | number, testAgainst: string | number, negation?: boolean): boolean;
    static contains(value: string | number, testAgainst: string | number, negation?: boolean): boolean;
    static isIn(values: string | number, testAgainst: Array<string | number> | string, negation?: boolean, splitter?: string): boolean;
    static startsWith(value: string | number, testAgainst: string | number, negation?: boolean): boolean;
    static endsWith(value: string | number, testAgainst: string | number, negation?: boolean): boolean;
    static regexMatches(value: string | number, testAgainst: string | number, negation?: boolean): boolean;
    private static _returnNegationCheck;
}
