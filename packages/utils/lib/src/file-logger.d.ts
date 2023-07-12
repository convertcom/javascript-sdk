/**
 * @param {string} file
 * @param {module} fs
 * @param {string=} appendMethod Defaults to 'appendFileSync'
 * @example new FileLogger('./convert.log', require('fs'), 'appendFileSync')
 * @constructor
 */
declare class FileLogger {
    private _file;
    private _fs;
    private _appendMethod;
    /**
     * @param {string} file
     * @param {module} fs
     * @param {string=} appendMethod
     */
    constructor(file: string, fs: any, appendMethod?: string);
    private _write;
    /**
     * @param {Array<any>} args
     */
    log(...args: any): Promise<void>;
    /**
     * @param {Array<any>} args
     */
    info(...args: any): Promise<void>;
    /**
     * @param {Array<any>} args
     */
    debug(...args: any): Promise<void>;
    /**
     * @param {Array<any>} args
     */
    warn(...args: any): Promise<void>;
    /**
     * @param {Array<any>} args
     */
    error(...args: any): Promise<void>;
}
export { FileLogger };
