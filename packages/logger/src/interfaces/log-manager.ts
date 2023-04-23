/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {LogLevel} from '@convertcom/enums';
import {LogClientInterface} from './log-client';
import {LogMethodMapInterface} from './log-method-map';
export interface LogManagerInterface {
  log(level: LogLevel, ...args: any[]): void;

  trace(...args: any[]): void;

  debug(...args: any[]): void;

  info(...args: any[]): void;

  warn(...args: any[]): void;

  error(...args: any[]): void;

  addClient(
    client: LogClientInterface,
    level?: LogLevel,
    methodMap?: LogMethodMapInterface
  ): void;
}
