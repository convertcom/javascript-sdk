/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {
  VisitorSegments,
  ConfigResponseData,
  Path,
  VisitorTrackingEvents
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
    visitorId: string,
    eventRequest: VisitorTrackingEvents,
    segments?: VisitorSegments
  ): void;

  releaseQueue(reason?: string): Promise<any>;

  stopQueue(): void;

  startQueue(): void;

  enableTracking(): void;

  disableTracking(): void;

  setData(data: ConfigResponseData): void;

  getConfig(): Promise<ConfigResponseData>;
}
