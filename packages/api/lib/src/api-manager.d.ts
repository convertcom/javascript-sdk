/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import { ApiManagerInterface } from './interfaces/api-manager';
import { Config, ConfigData, Id, SegmentsData, VisitorEvent, Path } from '@convertcom/types';
import { LogManagerInterface } from '@convertcom/logger';
import { EventManagerInterface } from '@convertcom/event';
import { HttpResponse } from '@convertcom/utils';
/**
 * Provides logic for network requests. Reads remote configuration and sends tracking events to Convert server.
 * @category Modules
 * @constructor
 * @implements {ApiManagerInterface}
 */
export declare class ApiManager implements ApiManagerInterface {
    private _requestsQueue;
    private _requestsQueueTimerID;
    private readonly _configEndpoint;
    private readonly _trackEndpoint;
    private readonly _defaultHeaders;
    private _data;
    private _enrichData;
    private _loggerManager;
    private _eventManager;
    private _accountId;
    private _projectId;
    private _trackingEvent;
    readonly batchSize: number;
    readonly releaseInterval: number;
    /**
     * @param {Config=} config
     * @param {Object=} dependencies
     * @param {EventManagerInterface=} dependencies.eventManager
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    constructor(config?: Config, { eventManager, loggerManager }?: {
        eventManager?: EventManagerInterface;
        loggerManager?: LogManagerInterface;
    });
    /**
     * Send request to api server
     * @param method
     * @param path
     * @param data
     * @param headers
     * @return {Promise<HttpResponse>}
     */
    request(method: string, path: Path, data?: Record<string, any>, headers?: Record<string, any>): Promise<HttpResponse>;
    /**
     * Add request to queue for sending to server
     * @param {Id} visitorId
     * @param {VisitorEvent} eventRequest
     * @param {SegmentsData} segments
     */
    enqueue(visitorId: Id, eventRequest: VisitorEvent, segments?: SegmentsData): void;
    /**
     * Send queue to server
     * @param {string=} reason
     * @return {Promise<any>}
     */
    releaseQueue(reason?: string): Promise<any>;
    /**
     * Stop queue timer
     */
    stopQueue(): void;
    /**
     * Start queue timer
     */
    startQueue(): void;
    /**
     * Set data
     */
    setData(data: ConfigData): void;
    /**
     * Get config data by SDK key
     * @param {string} sdkKey
     * @return {Promise<ConfigData>}
     */
    getConfigByKey(sdkKey: string): Promise<ConfigData>;
}
