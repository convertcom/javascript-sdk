/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * Validates variable is array and not empty
 * @param array
 */
export function arrayNotEmpty(array: Array<unknown>): boolean {
  return Array.isArray(array) && array.length > 0;
}
