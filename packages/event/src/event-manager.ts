/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {EventManagerInterface} from './interfaces/event-manager';
import {Config} from '@convertcom/js-sdk-types';
import {SystemEvents} from '@convertcom/js-sdk-enums';
import {LogManagerInterface} from '@convertcom/js-sdk-logger';

/**
 * Event wrapper
 * @category Modules
 * @constructor
 * @implements {EventManagerInterface}
 */
export class EventManager implements EventManagerInterface {
  private _loggerManager: LogManagerInterface | null;

  _listeners: Record<string, Array<any>>;
  _deferred: Record<string, Record<string, unknown>>;

  /**
   * @param {Config} config
   * @param {Object} dependencies
   * @param {LogManagerInterface=} dependencies.loggerManager
   */
  constructor(
    config?: Config,
    {loggerManager}: {loggerManager?: LogManagerInterface} = {}
  ) {
    this._listeners = {};
    this._deferred = {};

    this._loggerManager = loggerManager;
  }

  /**
   * Add listener for event
   * @param {SystemEvents | string} event Event name
   * @param {function} fn Callback function
   */
  on(event: SystemEvents | string, fn: (args: any, err: any) => void): void {
    (this._listeners[event] = this._listeners[event] || []).push(fn);
    this._loggerManager?.trace?.('EventManage.on()', {event: event});
    if (Object.hasOwnProperty.call(this._deferred, event)) {
      this.fire(event, this._deferred[event].args, this._deferred[event].err);
    }
  }

  /**
   * Remove all listeners from event
   * @param event
   */
  removeListeners(event: string): void {
    if (Object.hasOwnProperty.call(this._listeners, event)) {
      delete this._listeners[event];
    }
    if (Object.hasOwnProperty.call(this._deferred, event)) {
      delete this._deferred[event];
    }
  }

  /**
   * Fire event with provided arguments and/or errors
   * @param {SystemEvents | string} event Event name
   * @param {Object=} args
   * @param {Error=} err
   * @param {boolean=} deferred Allows to fire listeners which were subscribed after event is fired
   */
  fire(
    event: SystemEvents | string,
    args: Record<string, unknown> | unknown = null,
    err: Error | any = null,
    deferred = false
  ): void {
    this._loggerManager?.trace?.('EventManage.fire()', {
      event: event,
      args: args,
      err: err,
      deferred: deferred
    });
    for (const k in this._listeners[event] || []) {
      if (
        Object.hasOwnProperty.call(this._listeners, event) &&
        typeof this._listeners[event][k] === 'function'
      ) {
        this._listeners[event][k].apply(null, [args, err]);
      }
    }
    if (deferred && !Object.hasOwnProperty.call(this._deferred, event)) {
      this._deferred[event] = {args, err};
    }
  }
}
