/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import { LogLevel } from '@convertcom/enums';
import { LogMethodMapInterface } from './log-method-map';
export interface LogClientInterface {
    sdk: any;
    level: LogLevel;
    mapper: LogMethodMapInterface;
}
