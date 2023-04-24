/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
/**
 * Returns the value at path of object
 * @param {Record<string, any>} object
 * @param {string} path
 * @param {any=} defaultValue
 * @param {boolean=} truthy Should Number 0 number and Boolean false be considered as normal value
 * @return {any}
 */
export declare function objectDeepValue(object: Record<string, any>, path: string, defaultValue?: any, truthy?: boolean): any;
/**
 * Deep merge objects and their keys and nested objects
 * Accepts arrays
 *
 * @param {...Record<any, any>} objects Objects to merge
 * @return {Record<any, any>}
 */
export declare function objectDeepMerge(...objects: any[]): any;
/**
 * Validates variable is object and not empty
 * @param object
 */
export declare function objectNotEmpty(object: any): boolean;
