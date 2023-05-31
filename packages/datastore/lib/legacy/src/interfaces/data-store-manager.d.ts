/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
export interface DataStoreManagerInterface {
    readonly batchSize: number;
    readonly releaseInterval: number;
    set(key: string, data: any): void;
    get(key: string): any;
    enqueue(key: string, data: any): void;
    releaseQueue(reason?: string): any;
    stopQueue(): void;
    startQueue(): void;
}
