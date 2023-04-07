import 'mocha';
import {expect} from 'chai';
import {assert} from 'chai';

import {BucketingManager as bm} from '../src/bucketing-manager';
import {RuleManager as rm} from '../src/rule-manager';
import {EventManager as em} from '../src/event-manager';
import {ApiManager as am} from '../src/api-manager';
import {DataManager as dm} from '../src/data-manager';
import {SegmentsManager as sm} from '../src/segments-manager';
import testConfig from './test-config.json';
import {Config} from '../src/config';

const host = 'http://localhost';
const port = 8090;
const release_timeout = 1000;
const test_timeout = release_timeout + 1000;
const batch_size = 5;

const configuration = Config({
  ...testConfig,
  api: {
    endpoint: {
      config: host + ':' + port,
      track: host + ':' + port
    }
  },
  events: {
    batch_size: batch_size,
    release_interval: release_timeout
  }
});
const bucketingManager = new bm(configuration);
const ruleManager = new rm(configuration);
const eventManager = new em(configuration);
const apiManager = new am(configuration, {eventManager});

describe('SegmentsManager tests', function () {
  const visitorId = 'XXX';
  let dataManager, segmentsManager;
  // eslint-disable-next-line mocha/no-hooks-for-single-case
  before(function () {
    dataManager = new dm(configuration, {
      bucketingManager,
      ruleManager,
      eventManager,
      apiManager
    });
    segmentsManager = new sm(configuration, {dataManager, ruleManager});
  });
  it('Should expose SegmentsManager', function () {
    assert.isDefined(sm);
  });
  it('Imported entity should be a constructor of SegmentsManager instance', function () {
    expect(sm)
      .to.be.a('function')
      .that.has.property('name')
      .which.equal('SegmentsManager');
  });
  it('Should successfully create new SegmentsManager instance', async function () {
    expect(segmentsManager)
      .to.be.an('object')
      .that.has.property('constructor')
      .that.has.property('name')
      .which.equal('SegmentsManager');
  });

  describe('Test Segments Manager', function () {
    it('Should successfully update segments in DataStore', function () {
      const segments = {country: 'US'};
      segmentsManager.putSegments(visitorId, segments);
      const localSegments = dataManager.getLocalStore(visitorId);
      expect(segments).to.deep.equal(localSegments?.segments);
    });
    it('Should successfully update custom segments for specific visitor', function () {
      const segmentKey = 'test-segments-1';
      const segmentId = '200299434';
      const updatedSegments = segmentsManager.selectCustomSegments(
        visitorId,
        segmentKey,
        {
          enabled: true
        }
      );
      expect(updatedSegments)
        .to.be.an('object')
        .that.has.property('customSegments')
        .to.deep.equal([segmentId]);
    });
    it('Should keep custom segments intact if already set for specific visitor', function () {
      const segmentKey = 'test-segments-1';
      const updatedSegments = segmentsManager.selectCustomSegments(
        visitorId,
        segmentKey,
        {
          enabled: true
        }
      );
      expect(updatedSegments).to.be.undefined;
    });
    it('Should keep custom segments intact if key is not found for specific visitor', function () {
      const segmentKey = 'test-segments-2';
      const updatedSegments = segmentsManager.selectCustomSegments(
        visitorId,
        segmentKey
      );
      expect(updatedSegments).to.be.undefined;
    });
  });
});
