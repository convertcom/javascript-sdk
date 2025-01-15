/* eslint-disable mocha/consistent-spacing-between-blocks */
import {expect} from 'chai';
import 'mocha';
import {Comparisons} from '../src/comparisons';

describe('Comparison Processor utils tests', function () {
  it('Equal should return true comparing numbers 123 and 123', function () {
    const result = Comparisons['equals'](123, 123);
    expect(result).to.equal(true);
  });
  it('Equal should return false comparing numbers 123 and 123 with negation', function () {
    const result = Comparisons['equals'](123, 123, true);
    expect(result).to.equal(false);
  });
  it('Equal should return false comparing numbers 321 and 123', function () {
    const result = Comparisons['equals'](321, 123);
    expect(result).to.equal(false);
  });
  it('Equal should return true comparing strings `value` and `value`', function () {
    const result = Comparisons['equals']('value', 'value');
    expect(result).to.equal(true);
  });
  it('Equal should return false comparing string `value` and number 123', function () {
    const result = Comparisons['equals']('value', 123);
    expect(result).to.equal(false);
  });
  it('Equal should return true comparing string `value` and number 123 with negation', function () {
    const result = Comparisons['equals']('value', 123, true);
    expect(result).to.equal(true);
  });
  it('Less should return true comparing string `122.5` and number 123', function () {
    const result = Comparisons['less']('122.5', 123);
    expect(result).to.equal(true);
  });
  it('Less should return true comparing string `1,123.5` and number 123 with negation', function () {
    const result = Comparisons['less']('1,123.5', 123, true);
    expect(result).to.equal(true);
  });
  it('Less should return true comparing numbers -111 against 123', function () {
    const result = Comparisons['less'](-111, 123);
    expect(result).to.equal(true);
  });
  it('Less should return false comparing numbers -111 against 123 with negation', function () {
    const result = Comparisons['less'](-111, 123, true);
    expect(result).to.equal(false);
  });
  it('Less should return true comparing numbers 123 against 123 with negation. (Greater case)', function () {
    const result = Comparisons['less'](123, 123, true);
    expect(result).to.equal(true);
  });
  it('Less should return false comparing numbers 321 against -123', function () {
    const result = Comparisons['less'](321, -123);
    expect(result).to.equal(false);
  });
  it('Less should return true comparing numbers 321 against -123 with negation', function () {
    const result = Comparisons['less'](321, -123, true);
    expect(result).to.equal(true);
  });
  it('Less should return true comparing strings `abcde` against `axyz`', function () {
    const result = Comparisons['less']('abcde', 'axyz');
    expect(result).to.equal(true);
  });
  it('Less should return false comparing strings `axyz` against `abcde`', function () {
    const result = Comparisons['less']('axyz', 'abcde');
    expect(result).to.equal(false);
  });
  it('Less should return false comparing number 4 against string `orange`', function () {
    const result = Comparisons['less'](4, 'orange');
    expect(result).to.equal(false);
  });
  it('Less should return false comparing string `orange` against number 4', function () {
    const result = Comparisons['less']('orange', 4);
    expect(result).to.equal(false);
  });
  it('Less should return false comparing string `orange` against number 4 even with negation', function () {
    const result = Comparisons['less']('orange', 4, true);
    expect(result).to.equal(false);
  });
  it('Less should return false comparing numbers 4 against 4', function () {
    const result = Comparisons['less'](4, 4);
    expect(result).to.equal(false);
  });
  it('LessEqual should return true comparing numbers 4 against 4', function () {
    const result = Comparisons['lessEqual'](4, 4);
    expect(result).to.equal(true);
  });
  it('LessEqual should return false comparing number 4 against string `orange`', function () {
    const result = Comparisons['lessEqual'](4, 'orange');
    expect(result).to.equal(false);
  });
  it('LessEqual should return false comparing number 4 against string `orange` even with negation', function () {
    const result = Comparisons['lessEqual'](4, 'orange', true);
    expect(result).to.equal(false);
  });
  it('LessEqual should return true comparing numbers 4 against 123', function () {
    const result = Comparisons['lessEqual'](4, 123);
    expect(result).to.equal(true);
  });
  it('LessEqual should return false comparing numbers 123 against 4', function () {
    const result = Comparisons['lessEqual'](123, 4);
    expect(result).to.equal(false);
  });
  it('LessEqual should return true comparing numbers 123 against 4 with negation', function () {
    const result = Comparisons['lessEqual'](123, 4, true);
    expect(result).to.equal(true);
  });
  it('LessEqual should return false comparing strings `axyz` against `abcde`', function () {
    const result = Comparisons['lessEqual']('axyz', 'abcde');
    expect(result).to.equal(false);
  });
  it('LessEqual should return false comparing numbers 1234 against 1234 with negation. (GreaterThan case)', function () {
    const result = Comparisons['lessEqual'](1234, 1234, true);
    expect(result).to.equal(false);
  });
  it('LessEqual should return true comparing strings `abcde` against `axyz`', function () {
    const result = Comparisons['lessEqual']('abcde', 'axyz');
    expect(result).to.equal(true);
  });
  it('LessEqual should return true comparing string `122.5` and number 123', function () {
    const result = Comparisons['lessEqual']('122.5', 123);
    expect(result).to.equal(true);
  });
  it('LessEqual should return true comparing string `122.5` and number 122.5', function () {
    const result = Comparisons['lessEqual']('122.5', 122.5);
    expect(result).to.equal(true);
  });
  it('LessEqual should return false comparing string `123.5` and number 122.5', function () {
    const result = Comparisons['lessEqual']('123.5', 122.5);
    expect(result).to.equal(false);
  });
  it('LessEqual should return true comparing string `1,124,000.5` and number 1123.5 with negation', function () {
    const result = Comparisons['lessEqual']('1,124,000.5', 1123.5, true);
    expect(result).to.equal(true);
  });
  it('LessEqual should return false comparing string `1,123.5` and number 1123.5 with negation', function () {
    const result = Comparisons['lessEqual']('1,123.5', 1123.5, true);
    expect(result).to.equal(false);
  });
  it('Contains should return true comparing strings `abcde` against `a`', function () {
    const result = Comparisons['contains']('abcde', 'a');
    expect(result).to.equal(true);
  });
  it('Contains should return true comparing numbers 12345 against 23', function () {
    const result = Comparisons['contains'](12345, 23);
    expect(result).to.equal(true);
  });
  it('Contains should return false comparing numbers 23 against 12345', function () {
    const result = Comparisons['contains'](23, 12345);
    expect(result).to.equal(false);
  });
  it('Contains should return true comparing numbers 23 against 12345 with negation', function () {
    const result = Comparisons['contains'](23, 12345, true);
    expect(result).to.equal(true);
  });
  it('Contains should return true comparing strings `abcde` against empty string ``', function () {
    const result = Comparisons['contains']('abcde', '');
    expect(result).to.equal(true);
  });
  it('IsIn should return true comparing numbers 23 and 23', function () {
    const result = Comparisons['contains'](23, 23);
    expect(result).to.equal(true);
  });
  it('IsIn should return true comparing strings `a` against `a|b|c|d|e`', function () {
    const result = Comparisons['isIn']('a', 'a|b|c|d|e');
    expect(result).to.equal(true);
  });
  it('IsIn should return false comparing strings `a` against `a|b|c|d|e` with negation', function () {
    const result = Comparisons['isIn']('a', 'a|b|c|d|e', true);
    expect(result).to.equal(false);
  });
  it('IsIn should return true comparing strings `a|c` against `a|b|c|d|e`', function () {
    const result = Comparisons['isIn']('a|c', 'a|b|c|d|e');
    expect(result).to.equal(true);
  });
  it('IsIn should return false comparing array [`ab`,`cd`,`ef`] against string `orange`', function () {
    const result = Comparisons['isIn']('orange', ['ab', 'cd', 'ef']);
    expect(result).to.equal(false);
  });
  it('IsIn should return true comparing string `orange` against array [`ab`,`cd`,`ef`] with negation', function () {
    const result = Comparisons['isIn']('orange', ['ab', 'cd', 'ef'], true);
    expect(result).to.equal(true);
  });
  it('IsIn should return true comparing string `AB|ef` against  array [`ab`,`cd`,`ef`]', function () {
    const result = Comparisons['isIn']('ab|ef', ['ab', 'cd', 'ef']);
    expect(result).to.equal(true);
  });
  it('IsIn should return false comparing array [] against empty string ``', function () {
    const result = Comparisons['isIn']('', []);
    expect(result).to.equal(false);
  });
  it('IsIn should return false comparing string `ab|ef` against array [`ab`, `cd`, `ef`] because provided splitter is `,`', function () {
    const result = Comparisons['isIn']('ab|ef', ['ab', 'cd', 'ef'], false, ',');
    expect(result).to.equal(false);
  });
  it('IsIn should return false comparing strings `a|c` against `a|b|c|d|e` because provided splitter is `,`', function () {
    const result = Comparisons['isIn']('a|c', 'a|b|c|d|e', false, ',');
    expect(result).to.equal(false);
  });
  it('IsIn should return true comparing strings `a,c` against `a,b,c,d,e` with splitter as `,`', function () {
    const result = Comparisons['isIn']('a,c', 'a,b,c,d,e', false, ',');
    expect(result).to.equal(true);
  });
  it('IsIn should return true comparing string `ab,ef` against array [`ab`,`cd`,`ef`]  with splitter as `,`', function () {
    const result = Comparisons['isIn']('ab,ef', ['ab', 'cd', 'ef'], false, ',');
    expect(result).to.equal(true);
  });
  it('IsIn should return true comparing number 456 against array [123, 456, 789]', function () {
    const result = Comparisons['isIn'](456, [123, 456, 789]);
    expect(result).to.equal(true);
  });
  it('IsIn should return just false comparing number 456 against object cause object is not valid rule here.', function () {
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    const result = Comparisons['isIn'](456, {foo: 'bar'});
    expect(result).to.equal(false);
  });
  it('StartsWith should return true comparing numbers 12345678 against 12', function () {
    const result = Comparisons['startsWith'](12345678, 12);
    expect(result).to.equal(true);
  });
  it('StartsWith should return true comparing strings `orange is fruit` against `orange`', function () {
    const result = Comparisons['startsWith']('orange is fruit`', 'orange');
    expect(result).to.equal(true);
  });
  it('StartsWith should return true comparing strings `oRaNgE is fruit` against `ORANGE`', function () {
    const result = Comparisons['startsWith']('orange is fruit`', 'orange');
    expect(result).to.equal(true);
  });
  it('StartsWith should return false comparing strings `orange is fruit` against `is`', function () {
    const result = Comparisons['startsWith']('orange is fruit`', 'is');
    expect(result).to.equal(false);
  });
  it('StartsWith should return true comparing strings `orange is fruit` against `is` with negation', function () {
    const result = Comparisons['startsWith']('orange is fruit`', 'is', true);
    expect(result).to.equal(true);
  });
  it('StartsWith should return true comparing strings `orange is fruit` against empty string ``', function () {
    const result = Comparisons['startsWith']('orange is fruit`', '');
    expect(result).to.equal(true);
  });
  it('EndsWith should return false comparing numbers 12345678 against 4567', function () {
    const result = Comparisons['endsWith'](12345678, 4567);
    expect(result).to.equal(false);
  });
  it('EndsWith should return true comparing numbers 12345678 against 4567 with negation', function () {
    const result = Comparisons['endsWith'](12345678, 4567, true);
    expect(result).to.equal(true);
  });
  it('EndsWith should return true comparing numbers 12345678 against 45678', function () {
    const result = Comparisons['endsWith'](12345678, 45678);
    expect(result).to.equal(true);
  });
  it('EndsWith should return false comparing numbers 12345678 against 45678 with negation', function () {
    const result = Comparisons['endsWith'](12345678, 45678, true);
    expect(result).to.equal(false);
  });
  it('EndsWith should return true comparing strings `orange is fruit` against `FRUIT`', function () {
    const result = Comparisons['endsWith']('orange is fruit', 'fruit');
    expect(result).to.equal(true);
  });
  it('EndsWith should return false comparing strings `orange is fruit` against `is`', function () {
    const result = Comparisons['endsWith']('orange is fruit`', 'is');
    expect(result).to.equal(false);
  });
  it('EndsWith should return true comparing strings `orange is fruit` against empty string ``', function () {
    const result = Comparisons['endsWith']('orange is fruit`', '');
    expect(result).to.equal(true);
  });
  it('regexMatches should return false matching `orange` with wrong regExp `/?wwww`', function () {
    const result = Comparisons['regexMatches']('/?wwww`', 'orange');
    expect(result).to.equal(false);
  });
  it('regexMatches should return true matching any string, eg. `orange` with regExp `\\w+`', function () {
    const result = Comparisons['regexMatches']('orange', '\\w+');
    expect(result).to.equal(true);
  });
  it('regexMatches should return true matching any string, eg. `An APPle!` with regExp `\\w+`', function () {
    const result = Comparisons['regexMatches']('An APPle!', '\\w+');
    expect(result).to.equal(true);
  });
  it('regexMatches should return false matching any characters, eg. `44orange` with regExp `\\d+`', function () {
    const result = Comparisons['regexMatches']('An APPle!', '\\d+');
    expect(result).to.equal(false);
  });
  it('regexMatches should return true matching any numbers, eg. 111222333 with regExp `\\d+`', function () {
    const result = Comparisons['regexMatches'](111222333, '\\d+');
    expect(result).to.equal(true);
  });
  it('regexMatches should return false matching any numbers, eg. 111222333 with regExp `\\d+` but with negation', function () {
    const result = Comparisons['regexMatches'](111222333, '\\d+', true);
    expect(result).to.equal(false);
  });
  /* eslint-disable */
  it("regexMatches should return true matching email, eg.`test@email.com` with regExp `^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$`", function () {
    const result = Comparisons['regexMatches'](
      'test@email.com',
      "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$"
    );
    expect(result).to.equal(true);
  });
  it("regexMatches should return true matching email, eg.`complex.email123@email.domain.com` with regExp `^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$", function () {
    const result = Comparisons['regexMatches'](
      'more.complex.e-mail123@subdomain.email.com',
      "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$"
    );
    expect(result).to.equal(true);
  });
  it("regexMatches should return false matching string `Not an email` with regExp `^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$", function () {
    const result = Comparisons['regexMatches'](
      'Not an email',
      "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$"
    );
    expect(result).to.equal(false);
  });
  it("regexMatches should return false matching string `wrongEmail.co@m` with regExp `^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$", function () {
    const result = Comparisons['regexMatches'](
      'wrong()\\Email.co@m',
      "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$"
    );
    expect(result).to.equal(false);
  });
  /* eslint-enable */
});
