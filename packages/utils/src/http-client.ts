/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {ERROR_MESSAGES, MESSAGES} from '@convertcom/enums';

export type HttpMethod =
  | 'GET'
  | 'DELETE'
  | 'HEAD'
  | 'OPTIONS'
  | 'POST'
  | 'PUT'
  | 'PATCH';

export type HttpResponseType = 'json' | 'arraybuffer' | 'text';

export type HttpRequest = {
  baseURL: string;
  path?: string;
  method?: HttpMethod;
  headers?: {
    [x: string]: any;
  };
  responseType?: HttpResponseType;
  data?: Record<string, any>;
};

enum HttpStatusCode {
  Continue = 100,
  SwitchingProtocols = 101,
  Processing = 102,
  EarlyHints = 103,
  Ok = 200,
  Created = 201,
  Accepted = 202,
  NonAuthoritativeInformation = 203,
  NoContent = 204,
  ResetContent = 205,
  PartialContent = 206,
  MultiStatus = 207,
  AlreadyReported = 208,
  ImUsed = 226,
  MultipleChoices = 300,
  MovedPermanently = 301,
  Found = 302,
  SeeOther = 303,
  NotModified = 304,
  UseProxy = 305,
  Unused = 306,
  TemporaryRedirect = 307,
  PermanentRedirect = 308,
  BadRequest = 400,
  Unauthorized = 401,
  PaymentRequired = 402,
  Forbidden = 403,
  NotFound = 404,
  MethodNotAllowed = 405,
  NotAcceptable = 406,
  ProxyAuthenticationRequired = 407,
  RequestTimeout = 408,
  Conflict = 409,
  Gone = 410,
  LengthRequired = 411,
  PreconditionFailed = 412,
  PayloadTooLarge = 413,
  UriTooLong = 414,
  UnsupportedMediaType = 415,
  RangeNotSatisfiable = 416,
  ExpectationFailed = 417,
  ImATeapot = 418,
  MisdirectedRequest = 421,
  UnprocessableEntity = 422,
  Locked = 423,
  FailedDependency = 424,
  TooEarly = 425,
  UpgradeRequired = 426,
  PreconditionRequired = 428,
  TooManyRequests = 429,
  RequestHeaderFieldsTooLarge = 431,
  UnavailableForLegalReasons = 451,
  InternalServerError = 500,
  NotImplemented = 501,
  BadGateway = 502,
  ServiceUnavailable = 503,
  GatewayTimeout = 504,
  HttpVersionNotSupported = 505,
  VariantAlsoNegotiates = 506,
  InsufficientStorage = 507,
  LoopDetected = 508,
  NotExtended = 510,
  NetworkAuthenticationRequired = 511
}

export type HttpResponse = {
  data: any;
  status: HttpStatusCode;
  statusText: string;
  headers?: {
    [x: string]: any;
  };
};

interface HttpClientInterface {
  request(config: HttpRequest): Promise<HttpResponse>;
}

let url, http, https, queryString;
try {
  // Gracefully attempt to NodeJS builtins, to prevent throwing exceptions in browsers
  url = require('url');
  http = require('http');
  https = require('https');
  queryString = require('querystring');
} catch (err) {
  // Should be browser env
}

const supportsRequestBody = (method: string) =>
  !['GET', 'HEAD', 'DELETE', 'TRACE', 'OPTIONS'].includes(method.toUpperCase());

const serialize = (params: Record<string, any>, method: string) => {
  let query = '';
  if (params && params.constructor === Object && !supportsRequestBody(method)) {
    if (typeof navigator !== 'undefined') {
      query = Object.keys(params)
        .map(
          (key) =>
            `${encodeURIComponent(key)}=${encodeURIComponent(params[key])}`
        )
        .join('&');
    } else {
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
export const HttpClient = {
  request(config: HttpRequest): Promise<HttpResponse> {
    const method = config?.method?.toUpperCase() || 'GET';
    const path = config?.path
      ? !config.path.startsWith('/')
        ? `/${config.path}`
        : config.path
      : '';
    const baseURL = config.baseURL.endsWith('/')
      ? config.baseURL.slice(0, -1)
      : config.baseURL;
    const responseType: HttpResponseType = config?.responseType || 'json';
    return new Promise((resolve, reject) => {
      if (typeof navigator !== 'undefined') {
        const options: any = {
          method
        };
        if (config?.headers) options.headers = config.headers;
        if (config?.data && supportsRequestBody(method)) {
          options.body = JSON.stringify(config.data);
        }
        const url = `${baseURL}${path}${serialize(config?.data, method)}`;
        if (method.toLowerCase() === 'post' && navigator?.sendBeacon) {
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
              statusText: MESSAGES.SEND_BEACON_SUCCESS
            });
          } else {
            reject({
              message: ERROR_MESSAGES.UNSUPPORTED_RESPONSE_TYPE
            });
          }
        } else {
          fetch(url, options)
            .then((res) => {
              if (res.status === HttpStatusCode.Ok) {
                const output: HttpResponse = {
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
                      message: ERROR_MESSAGES.UNSUPPORTED_RESPONSE_TYPE
                    });
                    return;
                }
                resolve(output);
              } else {
                reject({
                  message: res.statusText,
                  status: res.status
                });
              }
            })
            .catch((err) =>
              reject({
                message: err?.message,
                status: err?.status,
                statusText: err?.statusText
              })
            );
        }
      } else if (url && https && http) {
        // Fallback to CommonJS if not targeting a browser
        const parsedBaseUrl = url.parse(baseURL);
        if (parsedBaseUrl.port) {
          parsedBaseUrl.port = Number(parsedBaseUrl.port);
        } else {
          parsedBaseUrl.port = parsedBaseUrl.protocol === 'https:' ? 443 : 80;
        }
        const pathPrefix = parsedBaseUrl.path.endsWith('/')
          ? parsedBaseUrl.path.slice(0, -1)
          : parsedBaseUrl.path;
        const client = parsedBaseUrl.protocol === 'https:' ? https : http;
        const body = [];
        const options: any = {
          hostname: parsedBaseUrl.hostname,
          path: `${pathPrefix}${path}${serialize(config?.data, method)}`,
          port: parsedBaseUrl.port,
          method
        };
        const postData =
          config?.data && supportsRequestBody(method)
            ? JSON.stringify(config.data)
            : null;
        if (config?.headers) options.headers = config.headers;
        if (postData) {
          if (!options.headers) options.headers = {};
          options.headers['Content-Length'] = Buffer.byteLength(postData);
        }
        const req = client.request(options, (res) => {
          res.on('data', (chunk) => body.push(chunk));
          res.on('end', () => {
            if (res.statusCode === HttpStatusCode.Ok) {
              const buffer = Buffer.concat(body);
              const data = buffer.toString();
              const output: HttpResponse = {
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
                  output.data = buffer?.buffer;
                  break;
                case 'text':
                  output.data = res;
                  break;
                default:
                  reject({
                    message: ERROR_MESSAGES.UNSUPPORTED_RESPONSE_TYPE
                  });
                  return;
              }
              resolve(output);
            } else {
              reject({
                message: res.statusMessage,
                status: res.statusCode
              });
            }
          });
        });
        req.on('error', (err) =>
          reject({
            message: err?.message,
            status: err?.code,
            statusText: err?.statusText
          })
        );
        if (postData) req.write(postData);
        req.end();
      } else {
        reject({
          message: ERROR_MESSAGES.UNABLE_TO_PERFORM_NETWORK_REQUEST
        });
      }
    });
  }
} as HttpClientInterface;
