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
  private _file: string;
  private _fs;

  /**
   * @param {string} file
   * @param {module} fs
   */
  constructor(file: string, fs) {
    this._file = file;
    this._fs = fs;
    try {
      if (!this._fs?.existsSync?.(this._file)) {
        this._fs?.writeFileSync?.(this._file, '{}');
      }
    } catch (error) {
      console.error(error);
    }
  }

  /**
   * Get value by key
   * @param {string} key
   * @return {any}
   */
  get(key: string): any {
    try {
      const data = JSON.parse(this._fs?.readFileSync?.(this._file));
      return data[key];
    } catch (error) {
      console.error(error);
    }
  }

  /**
   * Store value by key
   * @param {string} key
   * @param {any} value
   */
  set(key: string, value: any) {
    try {
      const data = JSON.parse(this._fs?.readFileSync?.(this._file));
      data[key] = value;
      this._fs?.writeFileSync?.(this._file, JSON.stringify(data));
    } catch (error) {
      console.error(error);
    }
  }

  /**
   * Delete value by key
   * @param {string} key
   */
  delete(key: string) {
    try {
      const data = JSON.parse(this._fs?.readFileSync?.(this._file));
      delete data[key];
      this._fs?.writeFileSync?.(this._file, JSON.stringify(data));
    } catch (error) {
      console.error(error);
    }
  }
}

export {DataStore};
