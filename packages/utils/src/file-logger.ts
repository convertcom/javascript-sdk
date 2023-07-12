/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {LogMethod} from '@convertcom/js-sdk-enums';

/**
 * @param {string} file
 * @param {module} fs
 * @param {string=} appendMethod Defaults to 'appendFileSync'
 * @example new FileLogger('./convert.log', require('fs'), 'appendFileSync')
 * @constructor
 */
class FileLogger {
  private _file: string;
  private _fs;
  private _appendMethod: string;

  /**
   * @param {string} file
   * @param {module} fs
   * @param {string=} appendMethod
   */
  constructor(file: string, fs, appendMethod = 'appendFileSync') {
    this._file = file;
    this._fs = fs;
    this._appendMethod = appendMethod;
  }

  private async _write(method: string, ...args: any): Promise<void> {
    const prefix = `${new Date().toISOString()} [${method.toUpperCase()}]`;
    const output = `${prefix} ${args
      .map(JSON.stringify)
      .join(`\n${prefix} `)}\n`;
    try {
      this._fs?.[this._appendMethod]?.(this._file, output);
    } catch (error) {
      console.warn(error);
    }
  }

  /**
   * @param {Array<any>} args
   */
  async log(...args: any): Promise<void> {
    await this._write(LogMethod.LOG, ...args);
  }

  /**
   * @param {Array<any>} args
   */
  async info(...args: any): Promise<void> {
    await this._write(LogMethod.INFO, ...args);
  }

  /**
   * @param {Array<any>} args
   */
  async debug(...args: any): Promise<void> {
    await this._write(LogMethod.DEBUG, ...args);
  }

  /**
   * @param {Array<any>} args
   */
  async warn(...args: any): Promise<void> {
    await this._write(LogMethod.WARN, ...args);
  }

  /**
   * @param {Array<any>} args
   */
  async error(...args: any): Promise<void> {
    await this._write(LogMethod.ERROR, ...args);
  }
}

export {FileLogger};
