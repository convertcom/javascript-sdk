import {expect} from 'chai';
import 'mocha';
import * as arrayUtils from '../src/array-utils';

describe('Array utils tool tests', function () {
  it('Should return true for not an empty array', function () {
    const array = ['some', 'value', 22];
    const result = arrayUtils.arrayNotEmpty(array);
    expect(result).to.eql(true);
  });

  it('Should return true for not an empty array with one item', function () {
    const array = [0];
    const result = arrayUtils.arrayNotEmpty(array);
    expect(result).to.eql(true);
  });

  it('Should return true for not an empty array with one boolean item', function () {
    const array = [false];
    const result = arrayUtils.arrayNotEmpty(array);
    expect(result).to.eql(true);
  });

  it('Should return false for empty array', function () {
    const array = [];
    const result = arrayUtils.arrayNotEmpty(array);
    expect(result).to.eql(false);
  });

  it('Should return false for null', function () {
    const array = null;
    const result = arrayUtils.arrayNotEmpty(array);
    expect(result).to.eql(false);
  });

  it('Should return false for declared not defined variable', function () {
    let array;
    const result = arrayUtils.arrayNotEmpty(array);
    expect(result).to.eql(false);
  });

  it('Should return false for object as it is not an array', function () {
    const notArray = {
      key: 'value'
    };
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    const result = arrayUtils.arrayNotEmpty(notArray);
    expect(result).to.eql(false);
  });

  it('Should return false for string as it is not an array', function () {
    const notArray = 'A string';
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    const result = arrayUtils.arrayNotEmpty(notArray);
    expect(result).to.eql(false);
  });

  it('Should return false for integer as it is not an array', function () {
    const notArray = 0;
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    const result = arrayUtils.arrayNotEmpty(notArray);
    expect(result).to.eql(false);
  });
});
