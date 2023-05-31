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
var EventManager = /** @class */ (function () {
    /**
     * @param {Config} config
     * @param {Object} dependencies
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    function EventManager(config, _a) {
        var _b = _a === void 0 ? {} : _a, loggerManager = _b.loggerManager;
        this._listeners = {};
        this._deferred = {};
        this._loggerManager = loggerManager;
    }
    /**
     * Add listener for event
     * @param {SystemEvents | string} event Event name
     * @param {function} fn Callback function
     */
    EventManager.prototype.on = function (event, fn) {
        var _a, _b;
        (this._listeners[event] = this._listeners[event] || []).push(fn);
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'EventManage.on()', { event: event });
        if (Object.hasOwnProperty.call(this._deferred, event)) {
            this.fire(event, this._deferred[event].args, this._deferred[event].err);
        }
    };
    /**
     * Remove all listeners from event
     * @param event
     */
    EventManager.prototype.removeListeners = function (event) {
        if (Object.hasOwnProperty.call(this._listeners, event)) {
            delete this._listeners[event];
        }
        if (Object.hasOwnProperty.call(this._deferred, event)) {
            delete this._deferred[event];
        }
    };
    /**
     * Fire event with provided arguments and/or errors
     * @param {SystemEvents | string} event Event name
     * @param {Object=} args
     * @param {Error=} err
     * @param {boolean=} deferred Allows to fire listeners which were subscribed after event is fired
     */
    EventManager.prototype.fire = function (event, args, err, deferred) {
        var _a, _b;
        if (args === void 0) { args = null; }
        if (err === void 0) { err = null; }
        if (deferred === void 0) { deferred = false; }
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, 'EventManage.fire()', {
            event: event,
            args: args,
            err: err,
            deferred: deferred
        });
        for (var k in this._listeners[event] || []) {
            if (Object.hasOwnProperty.call(this._listeners, event) &&
                typeof this._listeners[event][k] === 'function') {
                this._listeners[event][k].apply(null, [args, err]);
            }
        }
        if (deferred && !Object.hasOwnProperty.call(this._deferred, event)) {
            this._deferred[event] = { args: args, err: err };
        }
    };
    return EventManager;
}());

exports.EventManager = EventManager;
//# sourceMappingURL=index.js.map
