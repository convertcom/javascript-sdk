/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * Returns the value at path of object
 * TODO: get this utility to work with the optional mapper() helper from config
 * @param {Record<string, any>} object
 * @param {string} path
 * @param {any=} defaultValue
 * @param {boolean=} truthy Should Number 0 number and Boolean false be considered as normal value
 * @return {any}
 */
export function objectDeepValue(
  object: Record<string, any>,
  path: string,
  // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
  defaultValue?: any,
  truthy = false
): any {
  try {
    if (typeof object === 'object') {
      const v = path.split('.').reduce((a, v) => a[v], object);
      if (v || (truthy && (v === false || v === 0))) {
        return v;
      }
    }
    // eslint-disable-next-line no-empty
  } catch (e) {}
  if (typeof defaultValue !== 'undefined') {
    return defaultValue;
  } else {
    return null;
  }
}

/**
 * Deep merge objects and their keys and nested objects
 * Accepts arrays
 *
 * @param {...Record<any, any>} objects Objects to merge
 * @return {Record<any, any>}
 */
export function objectDeepMerge(...objects) {
  const isObject = (obj) => obj && typeof obj === 'object';

  return objects.reduce((prev, obj) => {
    Object.keys(obj).forEach((key) => {
      const pVal = prev[key];
      const oVal = obj[key];

      if (Array.isArray(pVal) && Array.isArray(oVal)) {
        prev[key] = [...new Set([...oVal, ...pVal])];
      } else if (isObject(pVal) && isObject(oVal)) {
        prev[key] = objectDeepMerge(pVal, oVal);
      } else {
        prev[key] = oVal;
      }
    });

    return prev;
  }, {});
}

/**
 * Check if plain object
 * @param object
 */
export const isPlainObject = (object: any): boolean => {
  /**
   * Perfect performance but returns false if code runs
   * in an environment where the global Object has been overridden or shadowed.
   */
  // return object?.constructor === Object;

  /**
   * Works without checking the constructor directly, but with poor performance
   * due to the use of Object.getPrototypeOf and Object.prototype.toString.call().
   * These methods, while precise for type checking, can be performance-intensive
   * when called repeatedly in performance-critical or high-frequency scenarios.
   */
  // return (
  //   typeof object === 'object' &&
  //   object !== null &&
  //   Object.prototype.toString.call(object) === '[object Object]' && // check if an Object
  //   Object.getPrototypeOf(object) === Object.prototype // check if a plain object
  // );

  /**
   * A more performant approach without relying on the constructor property
   * or the more intensive checks like Object.getPrototypeOf and Object.prototype.toString.call().
   */
  if (typeof object !== 'object' || object === null) return false;
  // check if the object is not an Array or other built-in types.
  if (
    Array.isArray(object) ||
    object instanceof Date ||
    object instanceof RegExp
  )
    return false;
  // check for objects created with Object.create(null)
  if (Object.getPrototypeOf(object) === null) return true;
  // final attempt to catch objects that are likely "plain"
  let proto = object;
  while (Object.getPrototypeOf(proto) !== null) {
    proto = Object.getPrototypeOf(proto);
  }
  return Object.getPrototypeOf(object) === proto;
};

/**
 * Validates variable is object and not empty
 * @param object
 */
export function objectNotEmpty(object: any): boolean {
  return isPlainObject(object) && Object.keys(object).length > 0;
}

/**
 * Compare two objects
 * @param a
 * @param b
 */
export const objectDeepEqual = (a, b) => {
  if (a === b) return true;
  if (typeof a != 'object' || typeof b != 'object' || a == null || b == null)
    return false;
  const keysA = Object.keys(a),
    keysB = Object.keys(b);
  if (keysA.length != keysB.length) return false;
  for (const key of keysA) {
    if (!keysB.includes(key)) return false;

    if (typeof a[key] === 'function' || typeof b[key] === 'function') {
      if (a[key].toString() != b[key].toString()) return false;
    } else {
      if (!objectDeepEqual(a[key], b[key])) return false;
    }
  }
  return true;
};
