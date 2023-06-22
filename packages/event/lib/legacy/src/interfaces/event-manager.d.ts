/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import { SystemEvents } from '@convertcom/enums';
export interface EventManagerInterface {
    _listeners: Record<string, Array<any>>;
    _deferred: Record<string, Record<string, unknown>>;
    on(event: SystemEvents | string, fn: (args: any, err: any) => void): void;
    fire(event: SystemEvents | string, args?: Record<string, unknown>, err?: Error | unknown, deferred?: boolean): void;
    removeListeners(event: string): void;
}
