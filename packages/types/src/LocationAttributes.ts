/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {IdentityField} from './IndentityField';

export type LocationAttributes = {
  locationProperties?: Record<any, any>;
  identityField?: IdentityField;
  forceEvent?: boolean;
  /**
   * Suppress persisting the matched-locations list to the visitor store (in-memory
   * map and/or configured DataStore). Defaults to `true`. Location matching and the
   * LOCATION_ACTIVATED/LOCATION_DEACTIVATED events are unaffected by this flag. Used
   * by preview contexts (qs-02) to guarantee zero store writes.
   */
  enableStorage?: boolean;
  /**
   * Suppress the `SystemEvents.LOCATION_ACTIVATED`/`LOCATION_DEACTIVATED`
   * event fires below only -- location matching is unaffected. Defaults to
   * `false`. Independent of `enableStorage`/`enableTracking`: a normal silent
   * run (`enableTracking: false`) must still fire these events. Used by
   * preview contexts (qs-02) for zero-trace event suppression (AC5).
   */
  suppressEvents?: boolean;
};
