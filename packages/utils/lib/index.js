'use strict';

var enums = require('@convertcom/enums');

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
/**
 * Validates variable is array and not empty
 * @param array
 */
function arrayNotEmpty(array) {
    return Array.isArray(array) && array.length > 0;
}

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
var _a;
/**
 * Comparison Processor. Provides comparison methods for rules validation
 */
class Comparisons {
    static equals(value, testAgainst, negation) {
        if (Array.isArray(value))
            return this._returnNegationCheck(value.includes(testAgainst), negation);
        if (value.constructor === Object && typeof testAgainst === 'string')
            return this._returnNegationCheck(Object.keys(value).includes(testAgainst), negation);
        return this._returnNegationCheck(value === testAgainst, negation);
    }
    static less(value, testAgainst, negation) {
        if (typeof value !== typeof testAgainst || value === testAgainst) {
            return false;
        }
        return this._returnNegationCheck(value < testAgainst, negation);
    }
    static lessEqual(value, testAgainst, negation) {
        if (typeof value !== typeof testAgainst) {
            return false;
        }
        if (value === testAgainst) {
            return true;
        }
        return this._returnNegationCheck(value <= testAgainst, negation);
    }
    static contains(value, testAgainst, negation) {
        value = String(value);
        testAgainst = String(testAgainst);
        value = value.valueOf().toLowerCase();
        testAgainst = testAgainst.valueOf().toLowerCase();
        if (testAgainst.replace(/^([\s]*)|([\s]*)$/g, '').length === 0) {
            return this._returnNegationCheck(true, negation);
        }
        return this._returnNegationCheck(value.indexOf(testAgainst) !== -1, negation);
    }
    static isIn(values, testAgainst, negation = false, splitter = '|') {
        const matchedValuesArray = String(values)
            .split(splitter)
            .map((item) => {
            return String(item);
        });
        if (typeof testAgainst === 'string') {
            testAgainst = testAgainst.split(splitter);
        }
        if (!Array.isArray(testAgainst)) {
            testAgainst = [];
        }
        testAgainst = testAgainst.map((item) => {
            return String(item).valueOf().toLowerCase();
        });
        for (let i = 0; i < matchedValuesArray.length; i++) {
            if (testAgainst.indexOf(matchedValuesArray[i]) === -1 && !negation) {
                return false;
            }
            if (testAgainst.indexOf(matchedValuesArray[i]) !== -1 && negation) {
                return false;
            }
        }
        return true;
    }
    static startsWith(value, testAgainst, negation) {
        value = String(value).valueOf().toLowerCase();
        testAgainst = String(testAgainst).valueOf().toLowerCase();
        return this._returnNegationCheck(value.indexOf(testAgainst) === 0, negation);
    }
    static endsWith(value, testAgainst, negation) {
        value = String(value).valueOf().toLowerCase();
        testAgainst = String(testAgainst).valueOf().toLowerCase();
        return this._returnNegationCheck(value.indexOf(testAgainst, value.length - testAgainst.length) !== -1, negation);
    }
    static regexMatches(value, testAgainst, negation) {
        value = String(value).valueOf().toLowerCase();
        testAgainst = String(testAgainst).valueOf();
        const regExp = new RegExp(testAgainst, 'i');
        return this._returnNegationCheck(regExp.test(value), negation);
    }
    static _returnNegationCheck(value, negation = false) {
        if (negation) {
            return !value;
        }
        else {
            return value;
        }
    }
}
_a = Comparisons;
Comparisons.equalsNumber = _a.equals;
Comparisons.matches = _a.equals;

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
class DataStore {
    /**
     * @param {string} file
     * @param {module} fs
     */
    constructor(file, fs) {
        var _a, _b, _c, _d;
        this._file = file;
        this._fs = fs;
        try {
            if (!((_b = (_a = this._fs) === null || _a === void 0 ? void 0 : _a.existsSync) === null || _b === void 0 ? void 0 : _b.call(_a, this._file))) {
                (_d = (_c = this._fs) === null || _c === void 0 ? void 0 : _c.writeFileSync) === null || _d === void 0 ? void 0 : _d.call(_c, this._file, '{}');
            }
        }
        catch (error) {
            console.error(error);
        }
    }
    /**
     * Get value by key
     * @param {string} key
     * @return {any}
     */
    get(key) {
        var _a, _b;
        try {
            const data = JSON.parse((_b = (_a = this._fs) === null || _a === void 0 ? void 0 : _a.readFileSync) === null || _b === void 0 ? void 0 : _b.call(_a, this._file));
            return data[key];
        }
        catch (error) {
            console.error(error);
        }
    }
    /**
     * Store value by key
     * @param {string} key
     * @param {any} value
     */
    set(key, value) {
        var _a, _b, _c, _d;
        try {
            const data = JSON.parse((_b = (_a = this._fs) === null || _a === void 0 ? void 0 : _a.readFileSync) === null || _b === void 0 ? void 0 : _b.call(_a, this._file));
            data[key] = value;
            (_d = (_c = this._fs) === null || _c === void 0 ? void 0 : _c.writeFileSync) === null || _d === void 0 ? void 0 : _d.call(_c, this._file, JSON.stringify(data));
        }
        catch (error) {
            console.error(error);
        }
    }
    /**
     * Delete value by key
     * @param {string} key
     */
    delete(key) {
        var _a, _b, _c, _d;
        try {
            const data = JSON.parse((_b = (_a = this._fs) === null || _a === void 0 ? void 0 : _a.readFileSync) === null || _b === void 0 ? void 0 : _b.call(_a, this._file));
            delete data[key];
            (_d = (_c = this._fs) === null || _c === void 0 ? void 0 : _c.writeFileSync) === null || _d === void 0 ? void 0 : _d.call(_c, this._file, JSON.stringify(data));
        }
        catch (error) {
            console.error(error);
        }
    }
}

/******************************************************************************
Copyright (c) Microsoft Corporation.

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
***************************************************************************** */
/* global Reflect, Promise */


function __awaiter(thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
}

/**
 * @param {string} file
 * @param {module} fs
 * @param {string=} appendMethod Defaults to 'appendFileSync'
 * @example new FileLogger('./convert.log', require('fs'), 'appendFileSync')
 * @constructor
 */
class FileLogger {
    /**
     * @param {string} file
     * @param {module} fs
     * @param {string=} appendMethod
     */
    constructor(file, fs, appendMethod = 'appendFileSync') {
        this._file = file;
        this._fs = fs;
        this._appendMethod = appendMethod;
    }
    _write(method, ...args) {
        var _a, _b;
        return __awaiter(this, void 0, void 0, function* () {
            const prefix = `${new Date().toISOString()} [${method.toUpperCase()}]`;
            const output = `${prefix} ${args
                .map(JSON.stringify)
                .join(`\n${prefix} `)}\n`;
            try {
                (_b = (_a = this._fs) === null || _a === void 0 ? void 0 : _a[this._appendMethod]) === null || _b === void 0 ? void 0 : _b.call(_a, this._file, output);
            }
            catch (error) {
                console.warn(error);
            }
        });
    }
    /**
     * @param {Array<any>} args
     */
    log(...args) {
        return __awaiter(this, void 0, void 0, function* () {
            yield this._write(enums.LogMethod.LOG, ...args);
        });
    }
    /**
     * @param {Array<any>} args
     */
    info(...args) {
        return __awaiter(this, void 0, void 0, function* () {
            yield this._write(enums.LogMethod.INFO, ...args);
        });
    }
    /**
     * @param {Array<any>} args
     */
    debug(...args) {
        return __awaiter(this, void 0, void 0, function* () {
            yield this._write(enums.LogMethod.DEBUG, ...args);
        });
    }
    /**
     * @param {Array<any>} args
     */
    warn(...args) {
        return __awaiter(this, void 0, void 0, function* () {
            yield this._write(enums.LogMethod.WARN, ...args);
        });
    }
    /**
     * @param {Array<any>} args
     */
    error(...args) {
        return __awaiter(this, void 0, void 0, function* () {
            yield this._write(enums.LogMethod.ERROR, ...args);
        });
    }
}

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
var HttpStatusCode;
(function (HttpStatusCode) {
    HttpStatusCode[HttpStatusCode["Continue"] = 100] = "Continue";
    HttpStatusCode[HttpStatusCode["SwitchingProtocols"] = 101] = "SwitchingProtocols";
    HttpStatusCode[HttpStatusCode["Processing"] = 102] = "Processing";
    HttpStatusCode[HttpStatusCode["EarlyHints"] = 103] = "EarlyHints";
    HttpStatusCode[HttpStatusCode["Ok"] = 200] = "Ok";
    HttpStatusCode[HttpStatusCode["Created"] = 201] = "Created";
    HttpStatusCode[HttpStatusCode["Accepted"] = 202] = "Accepted";
    HttpStatusCode[HttpStatusCode["NonAuthoritativeInformation"] = 203] = "NonAuthoritativeInformation";
    HttpStatusCode[HttpStatusCode["NoContent"] = 204] = "NoContent";
    HttpStatusCode[HttpStatusCode["ResetContent"] = 205] = "ResetContent";
    HttpStatusCode[HttpStatusCode["PartialContent"] = 206] = "PartialContent";
    HttpStatusCode[HttpStatusCode["MultiStatus"] = 207] = "MultiStatus";
    HttpStatusCode[HttpStatusCode["AlreadyReported"] = 208] = "AlreadyReported";
    HttpStatusCode[HttpStatusCode["ImUsed"] = 226] = "ImUsed";
    HttpStatusCode[HttpStatusCode["MultipleChoices"] = 300] = "MultipleChoices";
    HttpStatusCode[HttpStatusCode["MovedPermanently"] = 301] = "MovedPermanently";
    HttpStatusCode[HttpStatusCode["Found"] = 302] = "Found";
    HttpStatusCode[HttpStatusCode["SeeOther"] = 303] = "SeeOther";
    HttpStatusCode[HttpStatusCode["NotModified"] = 304] = "NotModified";
    HttpStatusCode[HttpStatusCode["UseProxy"] = 305] = "UseProxy";
    HttpStatusCode[HttpStatusCode["Unused"] = 306] = "Unused";
    HttpStatusCode[HttpStatusCode["TemporaryRedirect"] = 307] = "TemporaryRedirect";
    HttpStatusCode[HttpStatusCode["PermanentRedirect"] = 308] = "PermanentRedirect";
    HttpStatusCode[HttpStatusCode["BadRequest"] = 400] = "BadRequest";
    HttpStatusCode[HttpStatusCode["Unauthorized"] = 401] = "Unauthorized";
    HttpStatusCode[HttpStatusCode["PaymentRequired"] = 402] = "PaymentRequired";
    HttpStatusCode[HttpStatusCode["Forbidden"] = 403] = "Forbidden";
    HttpStatusCode[HttpStatusCode["NotFound"] = 404] = "NotFound";
    HttpStatusCode[HttpStatusCode["MethodNotAllowed"] = 405] = "MethodNotAllowed";
    HttpStatusCode[HttpStatusCode["NotAcceptable"] = 406] = "NotAcceptable";
    HttpStatusCode[HttpStatusCode["ProxyAuthenticationRequired"] = 407] = "ProxyAuthenticationRequired";
    HttpStatusCode[HttpStatusCode["RequestTimeout"] = 408] = "RequestTimeout";
    HttpStatusCode[HttpStatusCode["Conflict"] = 409] = "Conflict";
    HttpStatusCode[HttpStatusCode["Gone"] = 410] = "Gone";
    HttpStatusCode[HttpStatusCode["LengthRequired"] = 411] = "LengthRequired";
    HttpStatusCode[HttpStatusCode["PreconditionFailed"] = 412] = "PreconditionFailed";
    HttpStatusCode[HttpStatusCode["PayloadTooLarge"] = 413] = "PayloadTooLarge";
    HttpStatusCode[HttpStatusCode["UriTooLong"] = 414] = "UriTooLong";
    HttpStatusCode[HttpStatusCode["UnsupportedMediaType"] = 415] = "UnsupportedMediaType";
    HttpStatusCode[HttpStatusCode["RangeNotSatisfiable"] = 416] = "RangeNotSatisfiable";
    HttpStatusCode[HttpStatusCode["ExpectationFailed"] = 417] = "ExpectationFailed";
    HttpStatusCode[HttpStatusCode["ImATeapot"] = 418] = "ImATeapot";
    HttpStatusCode[HttpStatusCode["MisdirectedRequest"] = 421] = "MisdirectedRequest";
    HttpStatusCode[HttpStatusCode["UnprocessableEntity"] = 422] = "UnprocessableEntity";
    HttpStatusCode[HttpStatusCode["Locked"] = 423] = "Locked";
    HttpStatusCode[HttpStatusCode["FailedDependency"] = 424] = "FailedDependency";
    HttpStatusCode[HttpStatusCode["TooEarly"] = 425] = "TooEarly";
    HttpStatusCode[HttpStatusCode["UpgradeRequired"] = 426] = "UpgradeRequired";
    HttpStatusCode[HttpStatusCode["PreconditionRequired"] = 428] = "PreconditionRequired";
    HttpStatusCode[HttpStatusCode["TooManyRequests"] = 429] = "TooManyRequests";
    HttpStatusCode[HttpStatusCode["RequestHeaderFieldsTooLarge"] = 431] = "RequestHeaderFieldsTooLarge";
    HttpStatusCode[HttpStatusCode["UnavailableForLegalReasons"] = 451] = "UnavailableForLegalReasons";
    HttpStatusCode[HttpStatusCode["InternalServerError"] = 500] = "InternalServerError";
    HttpStatusCode[HttpStatusCode["NotImplemented"] = 501] = "NotImplemented";
    HttpStatusCode[HttpStatusCode["BadGateway"] = 502] = "BadGateway";
    HttpStatusCode[HttpStatusCode["ServiceUnavailable"] = 503] = "ServiceUnavailable";
    HttpStatusCode[HttpStatusCode["GatewayTimeout"] = 504] = "GatewayTimeout";
    HttpStatusCode[HttpStatusCode["HttpVersionNotSupported"] = 505] = "HttpVersionNotSupported";
    HttpStatusCode[HttpStatusCode["VariantAlsoNegotiates"] = 506] = "VariantAlsoNegotiates";
    HttpStatusCode[HttpStatusCode["InsufficientStorage"] = 507] = "InsufficientStorage";
    HttpStatusCode[HttpStatusCode["LoopDetected"] = 508] = "LoopDetected";
    HttpStatusCode[HttpStatusCode["NotExtended"] = 510] = "NotExtended";
    HttpStatusCode[HttpStatusCode["NetworkAuthenticationRequired"] = 511] = "NetworkAuthenticationRequired";
})(HttpStatusCode || (HttpStatusCode = {}));
let url, http, https, queryString;
try {
    // Gracefully attempt to NodeJS builtins, to prevent throwing exceptions in browsers
    url = require('url');
    http = require('http');
    https = require('https');
    queryString = require('querystring');
}
catch (err) {
    // Should be browser env
}
const supportsRequestBody = (method) => !['GET', 'HEAD', 'DELETE', 'TRACE', 'OPTIONS'].includes(method.toUpperCase());
const serialize = (params, method) => {
    let query = '';
    if (params && params.constructor === Object && !supportsRequestBody(method)) {
        if (typeof navigator !== 'undefined') {
            query = Object.keys(params)
                .map((key) => `${encodeURIComponent(key)}=${encodeURIComponent(params[key])}`)
                .join('&');
        }
        else {
            query = queryString.stringify(params);
        }
    }
    return query ? `?${query}` : query;
};
/**
 * Provide http client for newtork requests
 * @param {HttpRequest}
 * @returns {HttpClient}
 */
const HttpClient = {
    request(config) {
        var _a;
        const method = ((_a = config === null || config === void 0 ? void 0 : config.method) === null || _a === void 0 ? void 0 : _a.toUpperCase()) || 'GET';
        const path = (config === null || config === void 0 ? void 0 : config.path)
            ? !config.path.startsWith('/')
                ? `/${config.path}`
                : config.path
            : '';
        const baseURL = config.baseURL.endsWith('/')
            ? config.baseURL.slice(0, -1)
            : config.baseURL;
        const responseType = (config === null || config === void 0 ? void 0 : config.responseType) || 'json';
        return new Promise((resolve, reject) => {
            if (typeof navigator !== 'undefined') {
                const options = {
                    method
                };
                if (config === null || config === void 0 ? void 0 : config.headers)
                    options.headers = config.headers;
                if ((config === null || config === void 0 ? void 0 : config.data) && supportsRequestBody(method)) {
                    options.body = JSON.stringify(config.data);
                }
                const url = `${baseURL}${path}${serialize(config === null || config === void 0 ? void 0 : config.data, method)}`;
                if (method.toLowerCase() === 'post' && (navigator === null || navigator === void 0 ? void 0 : navigator.sendBeacon)) {
                    /**
                     * navigator.sendBeacon method is intended for analytics
                     * and diagnostics code to send data to a server,
                     * given that analytics data are often sent to different
                     * subdomains or even different domains:
                     * 1. The browser drops CORS restraints resulted in omitting
                     * the OPTIONS request and allowing relevant cookies to be sent.
                     * 2. The browser will not abort the requests upon page unload,
                     * instead completes them in the background while the next page
                     * requests was already being processed.
                     * 3. The browser cannot decide whether the request has failed,
                     * the function only returns a boolean.
                     * 4. The specification does not define body size limitations,
                     * vendors may choose to limit the size of the request.
                     * 5. Only supports requests with POST method.
                     * 6. The following browsers cannot send Blob data: Chrome, Chrome Android, Opera, Opera Android, and WebView Android.
                     */
                    if (navigator.sendBeacon(url, options.body)) {
                        resolve({
                            data: true,
                            status: HttpStatusCode.Ok,
                            statusText: enums.MESSAGES.SEND_BEACON_SUCCESS
                        });
                    }
                    else {
                        reject({
                            message: enums.ERROR_MESSAGES.UNSUPPORTED_RESPONSE_TYPE
                        });
                    }
                }
                else {
                    fetch(url, options)
                        .then((res) => {
                        if (res.status === HttpStatusCode.Ok) {
                            const output = {
                                status: res.status,
                                statusText: res.statusText,
                                headers: res.headers,
                                data: null
                            };
                            switch (responseType) {
                                case 'json':
                                    output.data = res.json();
                                    break;
                                case 'arraybuffer':
                                    output.data = res.arrayBuffer();
                                    break;
                                case 'text':
                                    output.data = res;
                                    break;
                                default:
                                    reject({
                                        message: enums.ERROR_MESSAGES.UNSUPPORTED_RESPONSE_TYPE
                                    });
                                    return;
                            }
                            resolve(output);
                        }
                        else {
                            reject({
                                message: res.statusText,
                                status: res.status
                            });
                        }
                    })
                        .catch((err) => reject({
                        message: err === null || err === void 0 ? void 0 : err.message,
                        status: err === null || err === void 0 ? void 0 : err.status,
                        statusText: err === null || err === void 0 ? void 0 : err.statusText
                    }));
                }
            }
            else if (url && https && http) {
                // Fallback to CommonJS if not targeting a browser
                const parsedBaseUrl = url.parse(baseURL);
                if (parsedBaseUrl.port) {
                    parsedBaseUrl.port = Number(parsedBaseUrl.port);
                }
                else {
                    parsedBaseUrl.port = parsedBaseUrl.protocol === 'https:' ? 443 : 80;
                }
                const pathPrefix = parsedBaseUrl.path.endsWith('/')
                    ? parsedBaseUrl.path.slice(0, -1)
                    : parsedBaseUrl.path;
                const client = parsedBaseUrl.protocol === 'https:' ? https : http;
                const body = [];
                const options = {
                    hostname: parsedBaseUrl.hostname,
                    path: `${pathPrefix}${path}${serialize(config === null || config === void 0 ? void 0 : config.data, method)}`,
                    port: parsedBaseUrl.port,
                    method
                };
                const postData = (config === null || config === void 0 ? void 0 : config.data) && supportsRequestBody(method)
                    ? JSON.stringify(config.data)
                    : null;
                if (config === null || config === void 0 ? void 0 : config.headers)
                    options.headers = config.headers;
                if (postData) {
                    if (!options.headers)
                        options.headers = {};
                    options.headers['Content-Length'] = Buffer.byteLength(postData);
                }
                const req = client.request(options, (res) => {
                    res.on('data', (chunk) => body.push(chunk));
                    res.on('end', () => {
                        if (res.statusCode === HttpStatusCode.Ok) {
                            const buffer = Buffer.concat(body);
                            const data = buffer.toString();
                            const output = {
                                status: res.statusCode,
                                statusText: res.statusMessage,
                                headers: res.headers,
                                data: null
                            };
                            switch (responseType) {
                                case 'json':
                                    output.data = data ? JSON.parse(data) : '';
                                    break;
                                case 'arraybuffer':
                                    output.data = buffer === null || buffer === void 0 ? void 0 : buffer.buffer;
                                    break;
                                case 'text':
                                    output.data = res;
                                    break;
                                default:
                                    reject({
                                        message: enums.ERROR_MESSAGES.UNSUPPORTED_RESPONSE_TYPE
                                    });
                                    return;
                            }
                            resolve(output);
                        }
                        else {
                            reject({
                                message: res.statusMessage,
                                status: res.statusCode
                            });
                        }
                    });
                });
                req.on('error', (err) => reject({
                    message: err === null || err === void 0 ? void 0 : err.message,
                    status: err === null || err === void 0 ? void 0 : err.code,
                    statusText: err === null || err === void 0 ? void 0 : err.statusText
                }));
                if (postData)
                    req.write(postData);
                req.end();
            }
            else {
                reject({
                    message: enums.ERROR_MESSAGES.UNABLE_TO_PERFORM_NETWORK_REQUEST
                });
            }
        });
    }
};

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
function objectDeepValue(object, path, 
// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
defaultValue, truthy = false) {
    try {
        if (typeof object === 'object') {
            const v = path.split('.').reduce((a, v) => a[v], object);
            if (v || (truthy && (v === false || v === 0))) {
                return v;
            }
        }
        // eslint-disable-next-line no-empty
    }
    catch (e) { }
    if (typeof defaultValue !== 'undefined') {
        return defaultValue;
    }
    else {
        return null;
    }
}
/**
 * Deep merge objects and their keys and nested objects
 * Accepts arrays
 *
 * @param {...Record<any, any>} objects Objects to merge
 * @return {Record<any, any>}
 */
function objectDeepMerge(...objects) {
    const isObject = (obj) => obj && typeof obj === 'object';
    return objects.reduce((prev, obj) => {
        Object.keys(obj).forEach((key) => {
            const pVal = prev[key];
            const oVal = obj[key];
            if (Array.isArray(pVal) && Array.isArray(oVal)) {
                prev[key] = [...new Set([...oVal, ...pVal])];
            }
            else if (isObject(pVal) && isObject(oVal)) {
                prev[key] = objectDeepMerge(pVal, oVal);
            }
            else {
                prev[key] = oVal;
            }
        });
        return prev;
    }, {});
}
/**
 * Validates variable is object and not empty
 * @param object
 */
function objectNotEmpty(object) {
    return (object && object.constructor === Object && Object.keys(object).length > 0);
}

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
/**
 * String formatter tool. Use %s for string %d for digit and %j for JSON formatting
 * @param {string} template
 * @param {Array<string>} args
 * @return {string}
 */
function stringFormat(template, ...args) {
    if (args.length) {
        let i = 0;
        return template
            .replace(/(%?)(%([sj]))/g, (match, p1, p2, p3) => {
            if (p1) {
                return match;
            }
            const arg = args[i++];
            const val = typeof arg === 'function' ? arg() : arg;
            switch (p3) {
                case 's':
                    return String(val);
                case 'j':
                    return JSON.stringify(val);
            }
        })
            .replace('%%', '%');
    }
    return String(template);
}
/**
 * String formatter tool. Transforms a space-separated string into camelCase
 * @param {string} input
 * @return {string}
 */
function camelCase(input) {
    return input
        .replace(/(?:^\w|[A-Z]|\b\w)/g, function (word, index) {
        return index == 0 ? word.toLowerCase() : word.toUpperCase();
    })
        .replace(/\s+/g, '');
}

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
/**
 * Transform value type. Does not do any JSON validation
 * @param value
 * @param type
 */
function castType(value, type) {
    switch (type) {
        case 'boolean':
            if (value === 'true') {
                return true;
            }
            if (value === 'false') {
                return false;
            }
            return !!value;
        case 'float':
            if (value === true) {
                return 1;
            }
            if (value === false) {
                return 0;
            }
            return parseFloat(value);
        case 'json':
            if (typeof value === 'object') {
                return value;
            }
            else {
                try {
                    return JSON.parse(value);
                }
                catch (err) {
                    return String(value);
                }
            }
        case 'string':
            return String(value);
        case 'integer':
            if (value === true) {
                return 1;
            }
            if (value === false) {
                return 0;
            }
            return parseInt(value);
        default:
            return value;
    }
}

exports.Comparisons = Comparisons;
exports.DataStore = DataStore;
exports.FileLogger = FileLogger;
exports.HttpClient = HttpClient;
exports.arrayNotEmpty = arrayNotEmpty;
exports.camelCase = camelCase;
exports.castType = castType;
exports.objectDeepMerge = objectDeepMerge;
exports.objectDeepValue = objectDeepValue;
exports.objectNotEmpty = objectNotEmpty;
exports.serialize = serialize;
exports.stringFormat = stringFormat;
//# sourceMappingURL=index.js.map
