'use strict';

var jsSdkEnums = require('@convertcom/js-sdk-enums');

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
const DEFAULT_LOG_LEVEL = jsSdkEnums.LogLevel.TRACE;
/**
 * Provides logging logic
 * @category Modules
 * @constructor
 * @implements {LogManagerInterface}
 */
class LogManager {
    /**
     * @param {any} client
     * @param {LogLevel=} level
     * @param {LogMethodMapInterface=} mapper
     */
    constructor(client = console, level = DEFAULT_LOG_LEVEL, mapper) {
        this._defaultMapper = {
            [jsSdkEnums.LogMethod.LOG]: jsSdkEnums.LogMethod.LOG,
            [jsSdkEnums.LogMethod.DEBUG]: jsSdkEnums.LogMethod.DEBUG,
            [jsSdkEnums.LogMethod.INFO]: jsSdkEnums.LogMethod.INFO,
            [jsSdkEnums.LogMethod.WARN]: jsSdkEnums.LogMethod.WARN,
            [jsSdkEnums.LogMethod.ERROR]: jsSdkEnums.LogMethod.ERROR,
            [jsSdkEnums.LogMethod.TRACE]: jsSdkEnums.LogMethod.TRACE
        };
        this._clients = [];
        this.addClient(client, level, mapper);
    }
    _isValidLevel(level) {
        return Object.values(jsSdkEnums.LogLevel).includes(level);
    }
    _isValidMethod(method) {
        return Object.values(jsSdkEnums.LogMethod).includes(method);
    }
    _log(method, level, ...args) {
        this._clients.forEach((client) => {
            if (level >= client.level && jsSdkEnums.LogLevel.SILENT !== level) {
                const fn = client.sdk[client.mapper[method]];
                if (fn) {
                    fn.call(client.sdk, ...args);
                }
                else {
                    console.log(`Info: Unable to find method "${method}()" in client sdk:`, client.sdk.constructor.name);
                    console[method](...args);
                }
            }
        });
    }
    /**
     * @param {LogLevel} level
     * @param {Array<any>} args
     */
    log(level, ...args) {
        if (!this._isValidLevel(level)) {
            // throw new Error('Invalid Log Level');
            console.error('Invalid Log Level');
            return;
        }
        this._log(jsSdkEnums.LogMethod.LOG, level, ...args);
    }
    /**
     * @param {Array<any>} args
     */
    trace(...args) {
        this._log(jsSdkEnums.LogMethod.TRACE, jsSdkEnums.LogLevel.TRACE, ...args);
    }
    /**
     * @param {Array<any>} args
     */
    debug(...args) {
        this._log(jsSdkEnums.LogMethod.DEBUG, jsSdkEnums.LogLevel.DEBUG, ...args);
    }
    /**
     * @param {Array<any>} args
     */
    info(...args) {
        this._log(jsSdkEnums.LogMethod.INFO, jsSdkEnums.LogLevel.INFO, ...args);
    }
    /**
     * @param {Array<any>} args
     */
    warn(...args) {
        this._log(jsSdkEnums.LogMethod.WARN, jsSdkEnums.LogLevel.WARN, ...args);
    }
    /**
     * @param {Array<any>} args
     */
    error(...args) {
        this._log(jsSdkEnums.LogMethod.ERROR, jsSdkEnums.LogLevel.ERROR, ...args);
    }
    /**
     * @param {any=} client
     * @param {LogLevel=} level
     * @param {LogMethodMapInterface=} methodMap
     */
    addClient(client = console, level = DEFAULT_LOG_LEVEL, methodMap) {
        if (!client) {
            // throw new Error('Invalid Client SDK');
            console.error('Invalid Client SDK');
            return;
        }
        if (!this._isValidLevel(level)) {
            // throw new Error('Invalid Log Level');
            console.error('Invalid Log Level');
            return;
        }
        const mapper = Object.assign({}, this._defaultMapper);
        if (methodMap) {
            Object.keys(methodMap)
                .filter(this._isValidMethod)
                .forEach((method) => {
                mapper[method] = methodMap[method];
            });
        }
        this._clients.push({ sdk: client, level, mapper });
    }
    /**
     * @param {LogLevel=} level
     * @param {any=} client
     */
    setClientLevel(level, client) {
        if (!this._isValidLevel(level)) {
            // throw new Error('Invalid Log Level');
            console.error('Invalid Log Level');
            return;
        }
        if (client) {
            const clientIndex = this._clients.findIndex(({ sdk }) => sdk === client);
            if (clientIndex === -1) {
                console.error('Client SDK not found');
                return;
            }
            this._clients[clientIndex].level = level;
        }
        else {
            for (let i = 0, max = this._clients.length; i < max; i++)
                this._clients[i].level = level;
        }
    }
}

exports.LogManager = LogManager;
//# sourceMappingURL=index.js.map
