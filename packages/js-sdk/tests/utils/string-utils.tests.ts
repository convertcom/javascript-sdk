import {expect} from 'chai';
import 'mocha';
import {stringFormat} from '../../src/utils/string-utils';

describe('String formatting tool tests', function () {
  it('Should use template string', function () {
    const template = 'Lorem ipsum dolor sit amet';
    const res = stringFormat(template);
    expect(res).to.eql(template);
  });

  it('Should replace %s in template with provided string', function () {
    const template = 'Lorem %s dolor sit amet';
    const res = stringFormat(template, 'ipsum');
    expect(res).to.eql('Lorem ipsum dolor sit amet');
  });

  it('Should replace %s in template with provided string returned by function', function () {
    const template = 'Lorem %s dolor sit amet';
    const res = stringFormat(template, () => {
      return 'ipsum';
    });
    expect(res).to.eql('Lorem ipsum dolor sit amet');
  });

  it('Should replace multiple %s in template with provided strings', function () {
    const template = 'Lorem %s dolor %s amet';
    const res = stringFormat(template, 'ipsum', 'sit');
    expect(res).to.eql('Lorem ipsum dolor sit amet');
  });

  it('Should format JSON string with help of %j', function () {
    const template = '%j';
    const res = stringFormat(template, 'Lorem ipsum dolor sit amet');
    expect(res).to.eql('"Lorem ipsum dolor sit amet"');
  });

  it('Should format JSON array with help of %j', function () {
    const template = 'This is array: %j';
    const res = stringFormat(template, [1, 2, 3]);
    expect(res).to.eql('This is array: [1,2,3]');
  });

  it('Should not replace escaped %%s string', function () {
    const template = 'Lorem %s dolor %%s';
    const res = stringFormat(template, 'ipsum');
    expect(res).to.eql('Lorem ipsum dolor %s');
  });
});
