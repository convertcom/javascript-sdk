/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {Id} from '../types/Id';

export interface BucketingManagerInterface {
  selectBucket(
    buckets: Record<string, number>,
    value: number,
    redistribute?: number
  ): string | null;

  getValueVisitorBased(visitorId: Id, seed?: number): number;

  getBucketForVisitor(
    buckets: Record<string, number>,
    visitorId: Id,
    redistribute?: number,
    seed?: number
  ): string | null;
}
