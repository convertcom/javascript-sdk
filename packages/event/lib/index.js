'use strict';

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
/**
 * Event wrapper
 * @category Modules
 * @constructor
 * @implements {EventManagerInterface}
 */
class EventManager {
    /**
     * @param {Config} config
     * @param {Object} dependencies
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    constructor(config, { loggerManager } = {}) {
        this._listeners = {};
        this._deferred = {};
        this._loggerManager = loggerManager;
    }
    /**
     * Add listener for event
     * @param {SystemEvents | string} event Event name
     * @param {function} fn Callback function
     */
    on(event, fn) {
        var _a, _b;
        (this._listeners[event] = this._listeners[event] || []).push(fn);
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'EventManage.on()', { event: event });
        if (Object.hasOwnProperty.call(this._deferred, event)) {
            this.fire(event, this._deferred[event].args, this._deferred[event].err);
        }
    }
    /**
     * Remove all listeners from event
     * @param event
     */
    removeListeners(event) {
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
    fire(event, args = null, err = null, deferred = false) {
        var _a, _b;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'EventManage.fire()', {
            event: event,
            args: args,
            err: err,
            deferred: deferred
        });
        for (const k in this._listeners[event] || []) {
            if (Object.hasOwnProperty.call(this._listeners, event) &&
                typeof this._listeners[event][k] === 'function') {
                this._listeners[event][k].apply(null, [args, err]);
            }
        }
        if (deferred && !Object.hasOwnProperty.call(this._deferred, event)) {
            this._deferred[event] = { args, err };
        }
    }
}

exports.EventManager = EventManager;
//# sourceMappingURL=index.js.map
