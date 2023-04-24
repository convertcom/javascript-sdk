/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
/**
 * Example of Data Store implementation with simple database Based JSON file.
 * It's not recommended to use it in production as it can be memory and CPU intensive.
 * @param {string} file
 * @param {module} fs
 */
declare class DataStore {
    private _file;
    private _fs;
    /**
     * @param {string} file
     * @param {module} fs
     */
    constructor(file: string, fs: any);
    /**
     * Get value by key
     * @param {string} key
     * @return {any}
     */
    get(key: string): any;
    /**
     * Store value by key
     * @param {string} key
     * @param {any} value
     */
    set(key: string, value: any): void;
    /**
     * Delete value by key
     * @param {string} key
     */
    delete(key: string): void;
}
export { DataStore };
