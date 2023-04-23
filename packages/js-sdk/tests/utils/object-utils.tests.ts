import {expect} from 'chai';
import 'mocha';
import {objectDeepValue, objectDeepMerge} from '@convertcom/utils';

describe('Object utils tool tests', function () {
  it('Should return macthing value with provided path of object', function () {
    const obj = {
      api: {
        endpoint: 'Lorem ipsum dolor sit amet'
      }
    };
    const res = objectDeepValue(obj, 'api.endpoint');
    expect(res).to.eql(obj.api.endpoint);
  });
  it('Should return default value when path not found with provided path of object', function () {
    const obj = {
      api: {
        endpoint: 'Lorem ipsum dolor sit amet'
      }
    };
    const defaultValue = 'default value';
    const res = objectDeepValue(obj, 'api.notFound', defaultValue);
    expect(res).to.eql(defaultValue);
  });
  it('Should consiter the number zero as normal value with provided path of object', function () {
    const obj = {
      api: {
        maxResults: 0
      }
    };
    const res = objectDeepValue(obj, 'api.maxResults', 1, true);
    expect(res).to.eql(obj.api.maxResults);
  });
  it('Should consiter the boolean false as normal value with provided path of object', function () {
    const obj = {
      api: {
        hasLimit: false
      }
    };
    const res = objectDeepValue(obj, 'api.hasLimit', 0, true);
    expect(res).to.eql(obj.api.hasLimit);
  });
  it('Should merge objects and their keys and nested objects', function () {
    const obj1 = {
      api: {
        endpoint: 'Lorem ipsum dolor sit amet'
      }
    };
    const obj2 = {
      api: {
        maxResults: 3
      },
      test: true
    };
    const res = objectDeepMerge(obj1, obj2);
    expect(res).to.eql({
      api: {
        endpoint: 'Lorem ipsum dolor sit amet',
        maxResults: 3
      },
      test: true
    });
  });
});
