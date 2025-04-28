/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import Murmurhash from 'murmurhash';

/**
 * String formatter tool. Use %s for string %d for digit and %j for JSON formatting
 * @param {string} template
 * @param {Array<string>} args
 * @return {string}
 */
export function stringFormat(template: string, ...args: any[]): string {
  if (args.length) {
    let i = 0;
    return template
      .replace(
        /(%?)(%([sj]))/g,
        (match: string, p1: string, p2: string, p3: string): string => {
          if (p1) {
            return match;
          }
          const arg = args[i++];
          const val = typeof arg === 'function' ? arg() : arg;
          switch (p3) {
            case 's':
              return String(val);
            case 'j':
              return JSON.stringify(val);
          }
        }
      )
      .replace('%%', '%');
  }
  return String(template);
}

/**
 * String formatter tool. Transforms a space-separated string into camelCase
 * @param {string} input
 * @return {string}
 */
export function camelCase(input: string): string {
  return input
    .replace(/(?:^\w|[A-Z]|\b\w)/g, function (word, index) {
      return index === 0 ? word.toLowerCase() : word.toUpperCase();
    })
    .replace(/\s+/g, '');
}

/**
 * Generate numeric hash based on seed
 * @param {string} value
 * @param {number=} seed
 * @return {number}
 */
export function generateHash(value: string, seed = 9999): number {
  return Murmurhash.v3(String(value), seed);
}

/**
 * Check if a value is numeric
 * @param {string | number} value
 * @returns {boolean}
 */
export function isNumeric(value: string | number): boolean {
  const regex = /^-?(?:(?:\d{1,3}(?:,\d{3})+|\d+)(?:\.\d+)?|\.\d+)$/;
  if (typeof value === 'number') return Number.isFinite(value);
  if (typeof value !== 'string' || !regex.test(value)) return false;
  const num = parseFloat(value.replace(/,/g, ''));
  return Number.isFinite(num);
}

/**
 * Convert a string to a number
 * @param {string | number} value
 * @returns {number}
 */
export function toNumber(value: string | number): number {
  if (typeof value === 'number') {
    return value;
  }
  const parts = String(value).split(',');
  return parseFloat(
    parts[0] == '0'
      ? String(value).replace(/,/g, '.')
      : String(value).replace(/,/g, '')
  );
}
