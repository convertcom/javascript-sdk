/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {LogLevel, LogMethod} from '@convertcom/enums';
import {LogClientInterface} from './interfaces/log-client';
import {LogMethodMapInterface} from './interfaces/log-method-map';
import {LogManagerInterface} from './interfaces/log-manager';

const DEFAULT_LOG_LEVEL = LogLevel.TRACE;

/**
 * Provides logging logic
 * @category Modules
 * @constructor
 * @implements {LogManagerInterface}
 */
export class LogManager implements LogManagerInterface {
  private _clients: LogClientInterface[];
  private _defaultMapper: LogMethodMapInterface = <LogMethodMapInterface>{
    [LogMethod.LOG]: LogMethod.LOG,
    [LogMethod.DEBUG]: LogMethod.DEBUG,
    [LogMethod.INFO]: LogMethod.INFO,
    [LogMethod.WARN]: LogMethod.WARN,
    [LogMethod.ERROR]: LogMethod.ERROR
  };

  /**
   * @param {any} client
   * @param {LogLevel=} level
   * @param {LogMethodMapInterface=} mapper
   */
  constructor(
    client: any = console,
    level: LogLevel = DEFAULT_LOG_LEVEL,
    mapper?: LogMethodMapInterface
  ) {
    this._clients = [];
    this.addClient(client, level, mapper);
  }

  private _isValidLevel(level: any): boolean {
    return (<any>Object).values(LogLevel).includes(level);
  }

  private _isValidMethod(method: string): boolean {
    return (<any>Object).values(LogMethod).includes(method);
  }

  private _log(method: string, level: number, ...args: any[]): void {
    this._clients.forEach((client: LogClientInterface) => {
      if (level >= client.level && LogLevel.SILENT !== level) {
        const fn = client.sdk[client.mapper[method]];
        if (fn) {
          fn.call(client.sdk, ...args);
        } else {
          console.log(
            `Info: Unable to find method "${method}()" in client sdk:`,
            client.sdk.constructor.name
          );
          console[method](...args);
        }
      }
    });
  }

  /**
   * @param {LogLevel} level
   * @param {Array<any>} args
   */
  log(level: LogLevel, ...args: any[]): void {
    if (!this._isValidLevel(level)) {
      // throw new Error('Invalid Log Level');
      console.error('Invalid Log Level');
      return;
    }
    this._log(LogMethod.LOG, level, ...args);
  }

  /**
   * @param {Array<any>} args
   */
  trace(...args: any[]): void {
    this._log(LogMethod.LOG, LogLevel.TRACE, ...args);
  }

  /**
   * @param {Array<any>} args
   */
  debug(...args: any[]): void {
    this._log(LogMethod.DEBUG, LogLevel.DEBUG, ...args);
  }

  /**
   * @param {Array<any>} args
   */
  info(...args: any[]): void {
    this._log(LogMethod.INFO, LogLevel.INFO, ...args);
  }

  /**
   * @param {Array<any>} args
   */
  warn(...args: any[]): void {
    this._log(LogMethod.WARN, LogLevel.WARN, ...args);
  }

  /**
   * @param {Array<any>} args
   */
  error(...args: any[]): void {
    this._log(LogMethod.ERROR, LogLevel.ERROR, ...args);
  }

  /**
   * @param {any=} client
   * @param {LogLevel=} level
   * @param {LogMethodMapInterface=} methodMap
   */
  addClient(
    client: any = console,
    level: LogLevel = DEFAULT_LOG_LEVEL,
    methodMap?: LogMethodMapInterface
  ): void {
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
    const mapper: LogMethodMapInterface = {...this._defaultMapper};
    if (methodMap) {
      Object.keys(methodMap)
        .filter(this._isValidMethod)
        .forEach((method: string) => {
          mapper[method] = methodMap[method];
        });
    }
    this._clients.push(<LogClientInterface>{sdk: client, level, mapper});
  }

  /**
   * @param {LogLevel=} level
   * @param {any=} client
   */
  setClientLevel(level: LogLevel, client?: any): void {
    if (!this._isValidLevel(level)) {
      // throw new Error('Invalid Log Level');
      console.error('Invalid Log Level');
      return;
    }

    if (client) {
      const clientIndex = this._clients.findIndex(({sdk}) => sdk === client);
      if (clientIndex === -1) {
        console.error('Client SDK not found');
        return;
      }
      this._clients[clientIndex].level = level;
    } else {
      for (let i = 0, max = this._clients.length; i < max; i++)
        this._clients[i].level = level;
    }
  }
}
