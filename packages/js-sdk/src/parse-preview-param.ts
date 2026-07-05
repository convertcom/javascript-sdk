/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * Numeric-only segment pattern used to validate both the experienceId and
 * variationId parts of a preview param value.
 */
const NUMERIC_ONLY = /^\d+$/;

/**
 * Parse a preview param value of the form `"{experienceId}.{variationId}"`
 * (mirrors the web tracking script's force-param
 * `_conv_eforce={experienceId}.{variationId}`).
 *
 * Splits on the first dot only; both segments must be non-empty,
 * numeric-only strings. Any other shape (missing dot, extra dot, empty
 * segment, non-numeric segment, non-string input) returns `null`.
 *
 * Pure function: never throws, has no side effects, performs no logging.
 *
 * @param {string} value
 * @returns {{experienceId: string; variationId: string} | null}
 */
export function parsePreviewParam(
  value: string
): {experienceId: string; variationId: string} | null {
  if (typeof value !== 'string') return null;

  const dotIndex = value.indexOf('.');
  if (dotIndex === -1) return null;
  if (value.indexOf('.', dotIndex + 1) !== -1) return null;

  const experienceId = value.slice(0, dotIndex);
  const variationId = value.slice(dotIndex + 1);

  if (!NUMERIC_ONLY.test(experienceId) || !NUMERIC_ONLY.test(variationId)) {
    return null;
  }

  return {experienceId, variationId};
}
