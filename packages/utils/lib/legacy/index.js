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
/**
 * Comparison Processor. Provides comparison methods for rules validation
 */
var Comparisons = /** @class */ (function () {
    function Comparisons() {
    }
    Comparisons.equals = function (value, testAgainst, negation) {
        if (Array.isArray(value))
            return this._returnNegationCheck(value.includes(testAgainst), negation);
        if (value.constructor === Object && typeof testAgainst === 'string')
            return this._returnNegationCheck(Object.keys(value).includes(testAgainst), negation);
        return this._returnNegationCheck(value === testAgainst, negation);
    };
    Comparisons.less = function (value, testAgainst, negation) {
        if (typeof value !== typeof testAgainst || value === testAgainst) {
            return false;
        }
        return this._returnNegationCheck(value < testAgainst, negation);
    };
    Comparisons.lessEqual = function (value, testAgainst, negation) {
        if (typeof value !== typeof testAgainst) {
            return false;
        }
        if (value === testAgainst) {
            return true;
        }
        return this._returnNegationCheck(value <= testAgainst, negation);
    };
    Comparisons.contains = function (value, testAgainst, negation) {
        value = String(value);
        testAgainst = String(testAgainst);
        value = value.valueOf().toLowerCase();
        testAgainst = testAgainst.valueOf().toLowerCase();
        if (testAgainst.replace(/^([\s]*)|([\s]*)$/g, '').length === 0) {
            return this._returnNegationCheck(true, negation);
        }
        return this._returnNegationCheck(value.indexOf(testAgainst) !== -1, negation);
    };
    Comparisons.isIn = function (values, testAgainst, negation, splitter) {
        if (negation === void 0) { negation = false; }
        if (splitter === void 0) { splitter = '|'; }
        var matchedValuesArray = String(values)
            .split(splitter)
            .map(function (item) {
            return String(item);
        });
        if (typeof testAgainst === 'string') {
            testAgainst = testAgainst.split(splitter);
        }
        if (!Array.isArray(testAgainst)) {
            testAgainst = [];
        }
        testAgainst = testAgainst.map(function (item) {
            return String(item).valueOf().toLowerCase();
        });
        for (var i = 0; i < matchedValuesArray.length; i++) {
            if (testAgainst.indexOf(matchedValuesArray[i]) === -1 && !negation) {
                return false;
            }
            if (testAgainst.indexOf(matchedValuesArray[i]) !== -1 && negation) {
                return false;
            }
        }
        return true;
    };
    Comparisons.startsWith = function (value, testAgainst, negation) {
        value = String(value).valueOf().toLowerCase();
        testAgainst = String(testAgainst).valueOf().toLowerCase();
        return this._returnNegationCheck(value.indexOf(testAgainst) === 0, negation);
    };
    Comparisons.endsWith = function (value, testAgainst, negation) {
        value = String(value).valueOf().toLowerCase();
        testAgainst = String(testAgainst).valueOf().toLowerCase();
        return this._returnNegationCheck(value.indexOf(testAgainst, value.length - testAgainst.length) !== -1, negation);
    };
    Comparisons.regexMatches = function (value, testAgainst, negation) {
        value = String(value).valueOf().toLowerCase();
        testAgainst = String(testAgainst).valueOf();
        var regExp = new RegExp(testAgainst, 'i');
        return this._returnNegationCheck(regExp.test(value), negation);
    };
    Comparisons._returnNegationCheck = function (value, negation) {
        if (negation === void 0) { negation = false; }
        if (negation) {
            return !value;
        }
        else {
            return value;
        }
    };
    var _a;
    _a = Comparisons;
    Comparisons.equalsNumber = _a.equals;
    Comparisons.matches = _a.equals;
    return Comparisons;
}());

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
var DataStore = /** @class */ (function () {
    /**
     * @param {string} file
     * @param {module} fs
     */
    function DataStore(file, fs) {
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
    DataStore.prototype.get = function (key) {
        var _a, _b;
        try {
            var data = JSON.parse((_b = (_a = this._fs) === null || _a === void 0 ? void 0 : _a.readFileSync) === null || _b === void 0 ? void 0 : _b.call(_a, this._file));
            return data[key];
        }
        catch (error) {
            console.error(error);
        }
    };
    /**
     * Store value by key
     * @param {string} key
     * @param {any} value
     */
    DataStore.prototype.set = function (key, value) {
        var _a, _b, _c, _d;
        try {
            var data = JSON.parse((_b = (_a = this._fs) === null || _a === void 0 ? void 0 : _a.readFileSync) === null || _b === void 0 ? void 0 : _b.call(_a, this._file));
            data[key] = value;
            (_d = (_c = this._fs) === null || _c === void 0 ? void 0 : _c.writeFileSync) === null || _d === void 0 ? void 0 : _d.call(_c, this._file, JSON.stringify(data));
        }
        catch (error) {
            console.error(error);
        }
    };
    /**
     * Delete value by key
     * @param {string} key
     */
    DataStore.prototype.delete = function (key) {
        var _a, _b, _c, _d;
        try {
            var data = JSON.parse((_b = (_a = this._fs) === null || _a === void 0 ? void 0 : _a.readFileSync) === null || _b === void 0 ? void 0 : _b.call(_a, this._file));
            delete data[key];
            (_d = (_c = this._fs) === null || _c === void 0 ? void 0 : _c.writeFileSync) === null || _d === void 0 ? void 0 : _d.call(_c, this._file, JSON.stringify(data));
        }
        catch (error) {
            console.error(error);
        }
    };
    return DataStore;
}());

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

function __awaiter(thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
}

function __generator(thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (g && (g = 0, op[0] && (_ = 0)), _) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
}

function __read(o, n) {
    var m = typeof Symbol === "function" && o[Symbol.iterator];
    if (!m) return o;
    var i = m.call(o), r, ar = [], e;
    try {
        while ((n === void 0 || n-- > 0) && !(r = i.next()).done) ar.push(r.value);
    }
    catch (error) { e = { error: error }; }
    finally {
        try {
            if (r && !r.done && (m = i["return"])) m.call(i);
        }
        finally { if (e) throw e.error; }
    }
    return ar;
}

function __spreadArray(to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
}

/**
 * @param {string} file
 * @param {module} fs
 * @param {string=} appendMethod Defaults to 'appendFileSync'
 * @example new FileLogger('./convert.log', require('fs'), 'appendFileSync')
 * @constructor
 */
var FileLogger = /** @class */ (function () {
    /**
     * @param {string} file
     * @param {module} fs
     * @param {string=} appendMethod
     */
    function FileLogger(file, fs, appendMethod) {
        if (appendMethod === void 0) { appendMethod = 'appendFileSync'; }
        this._file = file;
        this._fs = fs;
        this._appendMethod = appendMethod;
    }
    FileLogger.prototype._write = function (method) {
        var _a, _b;
        var args = [];
        for (var _i = 1; _i < arguments.length; _i++) {
            args[_i - 1] = arguments[_i];
        }
        return __awaiter(this, void 0, void 0, function () {
            var prefix, output;
            return __generator(this, function (_c) {
                prefix = "".concat(new Date().toISOString(), " [").concat(method.toUpperCase(), "]");
                output = "".concat(prefix, " ").concat(args
                    .map(JSON.stringify)
                    .join("\n".concat(prefix, " ")), "\n");
                try {
                    (_b = (_a = this._fs) === null || _a === void 0 ? void 0 : _a[this._appendMethod]) === null || _b === void 0 ? void 0 : _b.call(_a, this._file, output);
                }
                catch (error) {
                    console.warn(error);
                }
                return [2 /*return*/];
            });
        });
    };
    /**
     * @param {Array<any>} args
     */
    FileLogger.prototype.log = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i] = arguments[_i];
        }
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this._write.apply(this, __spreadArray([enums.LogMethod.LOG], __read(args), false))];
                    case 1:
                        _a.sent();
                        return [2 /*return*/];
                }
            });
        });
    };
    /**
     * @param {Array<any>} args
     */
    FileLogger.prototype.info = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i] = arguments[_i];
        }
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this._write.apply(this, __spreadArray([enums.LogMethod.INFO], __read(args), false))];
                    case 1:
                        _a.sent();
                        return [2 /*return*/];
                }
            });
        });
    };
    /**
     * @param {Array<any>} args
     */
    FileLogger.prototype.debug = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i] = arguments[_i];
        }
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this._write.apply(this, __spreadArray([enums.LogMethod.DEBUG], __read(args), false))];
                    case 1:
                        _a.sent();
                        return [2 /*return*/];
                }
            });
        });
    };
    /**
     * @param {Array<any>} args
     */
    FileLogger.prototype.warn = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i] = arguments[_i];
        }
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this._write.apply(this, __spreadArray([enums.LogMethod.WARN], __read(args), false))];
                    case 1:
                        _a.sent();
                        return [2 /*return*/];
                }
            });
        });
    };
    /**
     * @param {Array<any>} args
     */
    FileLogger.prototype.error = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i] = arguments[_i];
        }
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, this._write.apply(this, __spreadArray([enums.LogMethod.ERROR], __read(args), false))];
                    case 1:
                        _a.sent();
                        return [2 /*return*/];
                }
            });
        });
    };
    return FileLogger;
}());

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
var url, http, https, queryString;
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
var supportsRequestBody = function (method) {
    return !['GET', 'HEAD', 'DELETE', 'TRACE', 'OPTIONS'].includes(method.toUpperCase());
};
var serialize = function (params, method) {
    var query = '';
    if (params && params.constructor === Object && !supportsRequestBody(method)) {
        if (typeof XMLHttpRequest !== 'undefined') {
            query = Object.keys(params)
                .map(function (key) {
                return "".concat(encodeURIComponent(key), "=").concat(encodeURIComponent(params[key]));
            })
                .join('&');
        }
        else {
            query = queryString.stringify(params);
        }
    }
    return query ? "?".concat(query) : query;
};
/**
 * Provide http client for newtork requests
 * @param {HttpRequest}
 * @returns {HttpClient}
 */
var HttpClient = {
    request: function (config) {
        var _a;
        var method = ((_a = config === null || config === void 0 ? void 0 : config.method) === null || _a === void 0 ? void 0 : _a.toUpperCase()) || 'GET';
        var path = (config === null || config === void 0 ? void 0 : config.path)
            ? !config.path.startsWith('/')
                ? "/".concat(config.path)
                : config.path
            : '';
        var baseURL = config.baseURL.endsWith('/')
            ? config.baseURL.slice(0, -1)
            : config.baseURL;
        var responseType = (config === null || config === void 0 ? void 0 : config.responseType) || 'json';
        return new Promise(function (resolve, reject) {
            if (typeof XMLHttpRequest !== 'undefined') {
                var options = {
                    method: method
                };
                if (config === null || config === void 0 ? void 0 : config.headers)
                    options.headers = config.headers;
                if ((config === null || config === void 0 ? void 0 : config.data) && supportsRequestBody(method)) {
                    options.body = JSON.stringify(config.data);
                }
                fetch("".concat(baseURL).concat(path).concat(serialize(config === null || config === void 0 ? void 0 : config.data, method)), options)
                    .then(function (res) {
                    if (res.status === HttpStatusCode.Ok) {
                        var output = {
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
                    .catch(function (err) {
                    return reject({
                        message: err === null || err === void 0 ? void 0 : err.message,
                        status: err === null || err === void 0 ? void 0 : err.status,
                        statusText: err === null || err === void 0 ? void 0 : err.statusText
                    });
                });
            }
            else if (url && https && http) {
                // Fallback to CommonJS if not targeting a browser
                var parsedBaseUrl = url.parse(baseURL);
                if (parsedBaseUrl.port) {
                    parsedBaseUrl.port = Number(parsedBaseUrl.port);
                }
                else {
                    parsedBaseUrl.port = parsedBaseUrl.protocol === 'https:' ? 443 : 80;
                }
                var pathPrefix = parsedBaseUrl.path.endsWith('/')
                    ? parsedBaseUrl.path.slice(0, -1)
                    : parsedBaseUrl.path;
                var client = parsedBaseUrl.protocol === 'https:' ? https : http;
                var body_1 = [];
                var options = {
                    hostname: parsedBaseUrl.hostname,
                    path: "".concat(pathPrefix).concat(path).concat(serialize(config === null || config === void 0 ? void 0 : config.data, method)),
                    port: parsedBaseUrl.port,
                    method: method
                };
                var postData = (config === null || config === void 0 ? void 0 : config.data) && supportsRequestBody(method)
                    ? JSON.stringify(config.data)
                    : null;
                if (config === null || config === void 0 ? void 0 : config.headers)
                    options.headers = config.headers;
                if (postData) {
                    if (!options.headers)
                        options.headers = {};
                    options.headers['Content-Length'] = Buffer.byteLength(postData);
                }
                var req = client.request(options, function (res) {
                    res.on('data', function (chunk) { return body_1.push(chunk); });
                    res.on('end', function () {
                        if (res.statusCode === HttpStatusCode.Ok) {
                            var buffer = Buffer.concat(body_1);
                            var data = buffer.toString();
                            var output = {
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
                req.on('error', function (err) {
                    return reject({
                        message: err === null || err === void 0 ? void 0 : err.message,
                        status: err === null || err === void 0 ? void 0 : err.code,
                        statusText: err === null || err === void 0 ? void 0 : err.statusText
                    });
                });
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
defaultValue, truthy) {
    if (truthy === void 0) { truthy = false; }
    try {
        if (typeof object === 'object') {
            var v = path.split('.').reduce(function (a, v) { return a[v]; }, object);
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
function objectDeepMerge() {
    var objects = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        objects[_i] = arguments[_i];
    }
    var isObject = function (obj) { return obj && typeof obj === 'object'; };
    return objects.reduce(function (prev, obj) {
        Object.keys(obj).forEach(function (key) {
            var pVal = prev[key];
            var oVal = obj[key];
            if (Array.isArray(pVal) && Array.isArray(oVal)) {
                prev[key] = __spreadArray([], __read(new Set(__spreadArray(__spreadArray([], __read(oVal), false), __read(pVal), false))), false);
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
function stringFormat(template) {
    var args = [];
    for (var _i = 1; _i < arguments.length; _i++) {
        args[_i - 1] = arguments[_i];
    }
    if (args.length) {
        var i_1 = 0;
        return template
            .replace(/(%?)(%([sj]))/g, function (match, p1, p2, p3) {
            if (p1) {
                return match;
            }
            var arg = args[i_1++];
            var val = typeof arg === 'function' ? arg() : arg;
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
exports.stringFormat = stringFormat;
//# sourceMappingURL=index.js.map
