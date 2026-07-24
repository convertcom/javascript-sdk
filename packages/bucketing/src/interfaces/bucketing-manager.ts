/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {
  BucketingAllocation,
  BucketingHash,
  VariationAllocation
} from '@convertcom/js-sdk-types';

/**
 * A precomputed anchored bucket range for a single variation.
 * `anchor` and `anchor + width` bound the half-open interval `[anchor, anchor + width)`
 * (per-10000 traffic space) that maps to `id` in the anchored layout.
 */
export type BucketAnchoredRange = {
  id: string;
  anchor: number;
  width: number;
};

export interface BucketingManagerInterface {
  selectBucket(
    buckets: Record<string, number>,
    value: number,
    redistribute?: number
  ): string | null;

  getValueVisitorBased(visitorId: string, options?: BucketingHash): number;

  getBucketForVisitor(
    buckets: Record<string, number>,
    visitorId: string,
    options?: BucketingHash
  ): BucketingAllocation | null;

  getBucketRanges(allocations: VariationAllocation[]): BucketAnchoredRange[];

  selectBucketAnchored(
    ranges: BucketAnchoredRange[],
    value: number
  ): string | null;

  getBucketForVisitorAnchored(
    allocations: VariationAllocation[],
    visitorId: string,
    options?: BucketingHash
  ): BucketingAllocation | null;
}
