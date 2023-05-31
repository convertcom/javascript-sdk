/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import { EventManagerInterface } from './interfaces/event-manager';
import { Config } from '@convertcom/types';
import { SystemEvents } from '@convertcom/enums';
import { LogManagerInterface } from '@convertcom/logger';
/**
 * Event wrapper
 * @category Modules
 * @constructor
 * @implements {EventManagerInterface}
 */
export declare class EventManager implements EventManagerInterface {
    private _loggerManager;
    _listeners: Record<string, Array<any>>;
    _deferred: Record<string, Record<string, unknown>>;
    /**
     * @param {Config} config
     * @param {Object} dependencies
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    constructor(config?: Config, { loggerManager }?: {
        loggerManager?: LogManagerInterface;
    });
    /**
     * Add listener for event
     * @param {SystemEvents | string} event Event name
     * @param {function} fn Callback function
     */
    on(event: SystemEvents | string, fn: (args: any, err: any) => void): void;
    /**
     * Remove all listeners from event
     * @param event
     */
    removeListeners(event: string): void;
    /**
     * Fire event with provided arguments and/or errors
     * @param {SystemEvents | string} event Event name
     * @param {Object=} args
     * @param {Error=} err
     * @param {boolean=} deferred Allows to fire listeners which were subscribed after event is fired
     */
    fire(event: SystemEvents | string, args?: Record<string, unknown> | unknown, err?: Error | any, deferred?: boolean): void;
}
