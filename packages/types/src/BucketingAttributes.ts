/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

export type BucketingAttributes = {
  environment?: string;
  locationProperties?: Record<any, any>;
  visitorProperties?: Record<any, any>;
  typeCasting?: boolean;
  experienceKeys?: Array<string>;
  updateVisitorProperties?: boolean;
  forceVariationId?: string;
  enableTracking?: boolean;
  /**
   * Suppress persisting the bucketing decision to the visitor store (in-memory
   * map and/or configured DataStore). Defaults to `true`. Used by preview
   * contexts (qs-02) to guarantee zero store writes.
   */
  enableStorage?: boolean;
  ignoreLocationProperties?: boolean;
  /**
   * Suppress the `SystemEvents.LOCATION_ACTIVATED`/`LOCATION_DEACTIVATED`
   * fires inside `selectLocations()` only -- location/audience matching is
   * unaffected. Defaults to `false`. Independent of `enableTracking`/
   * `enableStorage`: a normal silent run (`enableTracking: false`) must still
   * fire these events. Used by preview contexts (qs-02) for zero-trace event
   * suppression (AC5).
   */
  suppressEvents?: boolean;
};
