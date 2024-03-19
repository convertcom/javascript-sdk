import 'mocha';
import {expect} from 'chai';
import {assert} from 'chai';
import {BucketingManager as bm} from '../src/bucketing-manager';
import configuration from './test-config.json';
import {Config} from '@convertcom/js-sdk-types';

const testsAmount = 10000;
const DEFAULT_MAX_TRAFFIC = 10000;

const getTestResultsForVisitor = function (
  bucketingManager,
  testVariations,
  visitorId,
  amount = testsAmount
) {
  const results = {};
  for (let i = 0; i < amount; i++) {
    const variationId = bucketingManager.getBucketForVisitor(
      testVariations,
      visitorId
    );
    results[variationId] = (results[variationId] || 0) + 1;
  }
  return results;
};

describe('BucketingManager tests', function () {
  let bucketingManager;

  it('Should expose BucketingManager', function () {
    assert.isDefined(bm);
  });

  it('Imported entity should be a constructor of BucketingManager instance', function () {
    expect(bm)
      .to.be.a('function')
      .that.has.property('name')
      .which.equal('BucketingManager');
  });

  it('Should create new BucketingManager instance with default config', function () {
    const bucketingManager = new bm();
    expect(bucketingManager)
      .to.be.an('object')
      .that.has.property('constructor')
      .that.has.property('name')
      .which.equal('BucketingManager');
  });

  it('Should create new BucketingManager instance with provided config', function () {
    bucketingManager = new bm(configuration as unknown as Config);
    expect(bucketingManager)
      .to.be.an('object')
      .that.has.property('constructor')
      .that.has.property('name')
      .which.equal('BucketingManager');
  });

  it('Should select a bucket', function () {
    const testVariations = {
      '100234567': 30,
      '100234568': 30,
      '100234569': 30,
      '100234570': 10
    };
    const variationId1 = bucketingManager.selectBucket(testVariations, 100);
    const variationId2 = bucketingManager.selectBucket(testVariations, 200);
    expect(variationId1)
      .to.be.oneOf(Object.keys(testVariations))
      .and.equal(variationId2);
  });

  it('Should select another bucket', function () {
    const testVariations = {
      '100234567': 30,
      '100234568': 30,
      '100234569': 30,
      '100234570': 10
    };
    const variationId1 = bucketingManager.selectBucket(testVariations, 6000);
    const variationId2 = bucketingManager.selectBucket(testVariations, 6500);
    expect(variationId1)
      .to.be.oneOf(Object.keys(testVariations))
      .and.equal(variationId2);
  });

  it('Should not select a bucket and return null', function () {
    let testVariations = {
      '100234567': 0,
      '100234568': 0,
      '100234569': 0,
      '100234570': 0
    };
    let variationId = bucketingManager.selectBucket(testVariations, 6000);
    expect(variationId).to.equal(null);

    testVariations = {
      '100234567': 30,
      '100234568': 10,
      '100234569': 30,
      '100234570': 30
    };
    variationId = bucketingManager.selectBucket(
      testVariations,
      DEFAULT_MAX_TRAFFIC + 1
    );
    expect(variationId).to.equal(null);
  });

  it('Should return a value generated with help of murmurhash based on Visitor id', function () {
    const value = bucketingManager.getValueVisitorBased('100123456');
    expect(value).to.be.a('number');
  });

  it('Should return different values generated with help of murmurhash based on Visitor id with seeds', function () {
    const value1 = bucketingManager.getValueVisitorBased('100123456', {
      seed: 11223344
    });
    const value2 = bucketingManager.getValueVisitorBased('100123456', {
      seed: 99887766
    });
    expect(value2).to.not.equal(value1);
  });

  it('Should return the same bucket based on Visitor string for every attempt', function () {
    const testVariations = {
      '100234567': 10,
      '100234568': 30,
      '100234569': 60,
      '100234570': 0
    };
    const visitorId = '01ABCD';
    const results = getTestResultsForVisitor(
      bucketingManager,
      testVariations,
      visitorId
    );
    expect(Object.keys(results)).to.have.length(1);
    for (const [variationId, attempts] of Object.entries(results)) {
      expect(variationId).to.be.oneOf(Object.keys(testVariations));
      expect(attempts).to.be.equal(testsAmount);
    }
  });
});
