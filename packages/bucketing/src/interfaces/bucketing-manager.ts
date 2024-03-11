/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {BucketingHash, Id} from '@convertcom/js-sdk-types';

export interface BucketingManagerInterface {
  selectBucket(
    buckets: Record<string, number>,
    value: number,
    redistribute?: number
  ): string | null;

  getValueVisitorBased(visitorId: Id, options?: BucketingHash): number;

  getBucketForVisitor(
    buckets: Record<string, number>,
    visitorId: Id,
    options?: BucketingHash
  ): string | null;
}
