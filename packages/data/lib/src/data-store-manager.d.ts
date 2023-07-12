import { DataStoreManagerInterface } from './interfaces/data-store-manager';
import { LogManagerInterface } from '@convertcom/js-sdk-logger';
import { EventManagerInterface } from '@convertcom/js-sdk-event';
import { Config } from '@convertcom/js-sdk-types';
/**
 * Data Store wrapper
 * @category Modules
 * @constructor
 * @implements {DataStoreManagerInterface}
 */
export declare class DataStoreManager implements DataStoreManagerInterface {
    private _requestsQueue;
    private _requestsQueueTimerID;
    private _loggerManager;
    private _eventManager;
    readonly batchSize: number;
    readonly releaseInterval: number;
    private _dataStore;
    /**
     * @param {Config=} config
     * @param {Object=} dependencies
     * @param {Object=} dependencies.dataStore
     * @param {EventManagerInterface=} dependencies.eventManager
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    constructor(config?: Config, { dataStore, eventManager, loggerManager }?: {
        dataStore?: any;
        eventManager?: EventManagerInterface;
        loggerManager?: LogManagerInterface;
    });
    set(key: string, data: any): void;
    get(key: string): any;
    enqueue(key: string, data: any): void;
    releaseQueue(reason?: string): any;
    stopQueue(): void;
    startQueue(): void;
    /**
     * dataStore setter
     * @param {any=} dataStore
     */
    set dataStore(dataStore: any);
    /**
     * dataStore getter
     */
    get dataStore(): any;
    /**
     * Validates dataStore object
     * @param {any=} dataStore
     * @return {boolean}
     */
    isValidDataStore(dataStore: any): boolean;
}
