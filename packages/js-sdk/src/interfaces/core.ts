/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {SystemEvents} from '../enums/system-events';
import {Id} from '../types/Id';
import {ContextInterface} from './context';

export interface CoreInterface {
  createContext(
    visitorId: Id,
    visitorAttributes?: Record<any, any>
  ): ContextInterface;

  on(event: SystemEvents, fn: (args?, err?) => void): void;

  onReady(): Promise<void>;
}
