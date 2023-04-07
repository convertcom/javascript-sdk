/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {Id} from '../types/Id';
import {SegmentsData} from '../types/SegmentsData';
import {VisitorEvent} from '../types/tracking/VisitorEvent';
import {ConfigData} from '../types/Config';
import {Path} from '../types/Path';

export interface ApiManagerInterface {
  readonly batchSize: number;
  readonly releaseInterval: number;

  request(
    method: string,
    path: Path,
    data: Record<string, any>,
    headers: Record<string, any>
  ): Promise<any>;

  enqueue(
    visitorId: Id,
    eventRequest: VisitorEvent,
    segments?: SegmentsData
  ): void;

  releaseQueue(reason?: string): Promise<any>;

  stopQueue(): void;

  startQueue(): void;

  setData(data: ConfigData): void;

  getConfigByKey(sdkKey: string): Promise<ConfigData>;
}
