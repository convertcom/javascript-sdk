/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import { LogMethod } from '@convertcom/js-sdk-enums';
export interface LogMethodMapInterface {
    [LogMethod.LOG]?: string;
    [LogMethod.DEBUG]?: string;
    [LogMethod.INFO]?: string;
    [LogMethod.WARN]?: string;
    [LogMethod.ERROR]?: string;
}
