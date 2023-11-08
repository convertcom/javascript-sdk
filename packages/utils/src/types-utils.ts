/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * Transform value type. Does not do any JSON validation
 * @param value
 * @param type
 */
export function castType(value: any, type: string): any {
  switch (type) {
    case 'boolean':
      if (value === 'true') {
        return true;
      }
      if (value === 'false') {
        return false;
      }
      return !!value;
    case 'float':
      if (value === true) {
        return 1;
      }
      if (value === false) {
        return 0;
      }
      return parseFloat(value);
    case 'json':
      if (typeof value === 'object') {
        return value;
      } else {
        try {
          return JSON.parse(value);
        } catch (err) {
          return String(value);
        }
      }
    case 'string':
      return String(value);
    case 'integer':
      if (value === true) {
        return 1;
      }
      if (value === false) {
        return 0;
      }
      return parseInt(value);
    default:
      return value;
  }
}
