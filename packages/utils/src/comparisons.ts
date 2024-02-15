/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * Comparison Processor. Provides comparison methods for rules validation
 */
export class Comparisons {
  static equals(
    value:
      | string
      | number
      | boolean
      | Array<string | number | boolean>
      | Record<string, string | number | boolean>,
    testAgainst: string | number | boolean,
    negation?: boolean
  ): boolean {
    if (Array.isArray(value))
      return this._returnNegationCheck(
        value.indexOf(testAgainst) !== -1,
        negation
      );
    if (value?.constructor === Object && typeof testAgainst === 'string')
      return this._returnNegationCheck(
        Object.keys(value).indexOf(testAgainst) !== -1,
        negation
      );
    return this._returnNegationCheck(value === testAgainst, negation);
  }

  static equalsNumber = this.equals;
  static matches = this.equals;

  static less(
    value: string | number,
    testAgainst: string | number,
    negation?: boolean
  ): boolean {
    if (typeof value !== typeof testAgainst) {
      return false;
    }
    return this._returnNegationCheck(value < testAgainst, negation);
  }

  static lessEqual(
    value: string | number,
    testAgainst: string | number,
    negation?: boolean
  ): boolean {
    if (typeof value !== typeof testAgainst) {
      return false;
    }
    return this._returnNegationCheck(value <= testAgainst, negation);
  }

  static contains(
    value: string | number,
    testAgainst: string | number,
    negation?: boolean
  ): boolean {
    value = String(value);
    testAgainst = String(testAgainst);
    value = value.valueOf().toLowerCase();
    testAgainst = testAgainst.valueOf().toLowerCase();
    if (testAgainst.replace(/^([\s]*)|([\s]*)$/g, '').length === 0) {
      return this._returnNegationCheck(true, negation);
    }
    return this._returnNegationCheck(
      value.indexOf(testAgainst) !== -1,
      negation
    );
  }

  static isIn(
    values: string | number,
    testAgainst: Array<string | number> | string,
    negation = false,
    splitter = '|'
  ): boolean {
    const matchedValuesArray = String(values)
      .split(splitter)
      .map((item) => {
        return String(item);
      });
    if (typeof testAgainst === 'string') {
      testAgainst = testAgainst.split(splitter);
    }
    if (!Array.isArray(testAgainst)) {
      testAgainst = [];
    }
    testAgainst = testAgainst.map((item) => {
      return <string>String(item).valueOf().toLowerCase();
    });
    for (let i = 0; i < matchedValuesArray.length; i++) {
      if (testAgainst.indexOf(matchedValuesArray[i]) !== -1) {
        return this._returnNegationCheck(true, negation);
      }
    }
    return this._returnNegationCheck(false, negation);
  }

  static startsWith(
    value: string | number,
    testAgainst: string | number,
    negation?: boolean
  ): boolean {
    value = String(value).valueOf().toLowerCase();
    testAgainst = String(testAgainst).valueOf().toLowerCase();
    return this._returnNegationCheck(
      value.indexOf(testAgainst) === 0,
      negation
    );
  }

  static endsWith(
    value: string | number,
    testAgainst: string | number,
    negation?: boolean
  ): boolean {
    value = String(value).valueOf().toLowerCase();
    testAgainst = String(testAgainst).valueOf().toLowerCase();
    return this._returnNegationCheck(
      value.indexOf(testAgainst, value.length - testAgainst.length) !== -1,
      negation
    );
  }

  static regexMatches(
    value: string | number,
    testAgainst: string | number,
    negation?: boolean
  ): boolean {
    value = String(value).valueOf().toLowerCase();
    testAgainst = String(testAgainst).valueOf();
    const regExp = new RegExp(testAgainst, 'i');
    return this._returnNegationCheck(regExp.test(value), negation);
  }

  private static _returnNegationCheck(
    value: boolean,
    negation = false
  ): boolean {
    if (negation) {
      return !value;
    } else {
      return value;
    }
  }
}
