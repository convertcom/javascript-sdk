/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {
  Id,
  SegmentsData,
  VisitorEvent,
  ConfigData,
  Path
} from '@convertcom/js-sdk-types';

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

  enableTracking(): void;

  disableTracking(): void;

  setData(data: ConfigData): void;

  getConfigByKey(sdkKey: string): Promise<ConfigData>;
}
