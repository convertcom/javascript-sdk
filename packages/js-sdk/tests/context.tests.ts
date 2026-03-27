/* eslint-disable mocha/consistent-spacing-between-blocks */
import 'mocha';
import {expect} from 'chai';
import {assert} from 'chai';
import http from 'http';

import {BucketingManager as bm} from '@convertcom/js-sdk-bucketing';
import {RuleManager as rm} from '@convertcom/js-sdk-rules';
import {EventManager as em} from '@convertcom/js-sdk-event';
import {ApiManager as am} from '@convertcom/js-sdk-api';
import {DataManager as dm} from '@convertcom/js-sdk-data';
import {ExperienceManager as exm} from '@convertcom/js-sdk-experience';
import {FeatureManager as fm} from '../src/feature-manager';
import {SegmentsManager as sm} from '@convertcom/js-sdk-segments';
import {Context as c} from '../src/context';
import testConfig from './test-config.json';
import {Config as ConfigType} from '@convertcom/js-sdk-types';
import {objectDeepMerge} from '@convertcom/js-sdk-utils';
import {BucketingError, EntityType, RuleError} from '@convertcom/js-sdk-enums';
import {defaultConfig} from '../src/config/default';
import {
  getFeaturesWithStatuses,
  getMultipleFeatureWithStatus,
  getSingleFeatureWithStatus,
  getVariationsAcrossAllExperiences
} from './setup/shared';

const host = 'http://localhost';
const port = 8090;
const release_timeout = 1000;
const test_timeout = release_timeout + 1000;
const batch_size = 5;

const configuration = objectDeepMerge(testConfig, defaultConfig, {
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
}) as unknown as ConfigType;
const bucketingManager = new bm(configuration);
const ruleManager = new rm(configuration);
const eventManager = new em(configuration);
const apiManager = new am(configuration, {eventManager});
const mixedTypeConfiguration = objectDeepMerge(
  testConfig,
  defaultConfig,
  {
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
  },
  {
    data: {
      experiences: [
        {
          id: '100218248',
          name: 'Test Experience AB Web',
          key: 'test-experience-ab-web-1',
          type: 'a/b',
          version: 6,
          status: 'active',
          global_js: "var s = 'test_experience_web'; console.log(s);",
          global_css: '.test-style { display: initial; }',
          url: 'https://convert.com',
          integrations: [],
          environments: ['live', 'staging'],
          site_area: {
            OR: [
              {
                AND: [
                  {
                    OR_WHEN: [
                      {
                        rule_type: 'generic_key_value',
                        matching: {
                          match_type: 'matches',
                          negated: false
                        },
                        key: 'url',
                        value: 'https://convert.com/'
                      }
                    ]
                  }
                ]
              }
            ]
          },
          audiences: ['100299433'],
          goals: ['100215959', '100215960', '100215961'],
          settings: {
            matching_options: {
              audiences: 'any'
            }
          },
          variations: [
            {
              id: '100299464',
              name: 'Variation 1',
              status: 'running',
              is_baseline: true,
              changes: [],
              key: '100299464-variation-1',
              traffic_allocation: 100.0
            }
          ]
        }
      ]
    }
  }
) as unknown as ConfigType;
const ruleDataProviderConfiguration = objectDeepMerge(
  testConfig,
  defaultConfig,
  {
    data: {
      experiences: [
        {
          id: '300001',
          name: 'Test RuleData URL Experience',
          key: 'ruledata-url',
          type: 'a/b',
          version: 1,
          status: 'active',
          global_js: "var s = 'ruledata_url';",
          global_css: '.ruledata-url { display: none; }',
          url: 'https://convert.com',
          integrations: [],
          environments: ['live', 'staging'],
          site_area: {
            OR: [
              {
                AND: [
                  {
                    OR_WHEN: [
                      {
                        rule_type: 'url',
                        matching: {
                          match_type: 'equals',
                          negated: false
                        },
                        value: 'https://convert.com/'
                      }
                    ]
                  }
                ]
              }
            ]
          },
          variations: [
            {
              id: '300001-var-1',
              name: 'Variation URL',
              status: 'running',
              is_baseline: true,
              changes: [],
              key: '300001-var-1',
              traffic_allocation: 100.0
            }
          ]
        },
        {
          id: '300002',
          name: 'Test RuleData Cookie Experience',
          key: 'ruledata-cookie',
          type: 'a/b',
          version: 1,
          status: 'active',
          global_js: "var s = 'ruledata_cookie';",
          global_css: '.ruledata-cookie { display: none; }',
          url: 'https://convert.com',
          integrations: [],
          environments: ['live', 'staging'],
          site_area: {
            OR: [
              {
                AND: [
                  {
                    OR_WHEN: [
                      {
                        rule_type: 'cookie',
                        matching: {
                          match_type: 'equals',
                          negated: false
                        },
                        value: 'utm_source=google'
                      }
                    ]
                  }
                ]
              }
            ]
          },
          variations: [
            {
              id: '300002-var-1',
              name: 'Variation Cookie',
              status: 'running',
              is_baseline: true,
              changes: [],
              key: '300002-var-1',
              traffic_allocation: 100.0
            }
          ]
        },
        {
          id: '300003',
          name: 'Test RuleData Geo Experience',
          key: 'ruledata-geo',
          type: 'a/b',
          version: 1,
          status: 'active',
          global_js: "var s = 'ruledata_geo';",
          global_css: '.ruledata-geo { display: none; }',
          url: 'https://convert.com',
          integrations: [],
          environments: ['live', 'staging'],
          site_area: {
            OR: [
              {
                AND: [
                  {
                    OR_WHEN: [
                      {
                        rule_type: 'country',
                        matching: {
                          match_type: 'equals',
                          negated: false
                        },
                        value: 'US'
                      }
                    ]
                  }
                ]
              }
            ]
          },
          variations: [
            {
              id: '300003-var-1',
              name: 'Variation Geo',
              status: 'running',
              is_baseline: true,
              changes: [],
              key: '300003-var-1',
              traffic_allocation: 100.0
            }
          ]
        },
        {
          id: '300004',
          name: 'Test RuleData Browser Experience',
          key: 'ruledata-browser',
          type: 'a/b',
          version: 1,
          status: 'active',
          global_js: "var s = 'ruledata_browser';",
          global_css: '.ruledata-browser { display: none; }',
          url: 'https://convert.com',
          integrations: [],
          environments: ['live', 'staging'],
          site_area: {
            OR: [
              {
                AND: [
                  {
                    OR_WHEN: [
                      {
                        rule_type: 'browser_name',
                        matching: {
                          match_type: 'equals',
                          negated: false
                        },
                        value: 'chrome'
                      }
                    ]
                  }
                ]
              }
            ]
          },
          variations: [
            {
              id: '300004-var-1',
              name: 'Variation Browser',
              status: 'running',
              is_baseline: true,
              changes: [],
              key: '300004-var-1',
              traffic_allocation: 100.0
            }
          ]
        },
        {
          id: '300005',
          name: 'Test RuleData JS Condition Experience',
          key: 'ruledata-js-condition',
          type: 'a/b',
          version: 1,
          status: 'active',
          global_js: "var s = 'ruledata_js_condition';",
          global_css: '.ruledata-js-condition { display: none; }',
          url: 'https://convert.com',
          integrations: [],
          environments: ['live', 'staging'],
          site_area: {
            OR: [
              {
                AND: [
                  {
                    OR_WHEN: [
                      {
                        rule_type: 'js_condition',
                        matching: {
                          match_type: 'equals',
                          negated: false
                        },
                        value: true
                      }
                    ]
                  }
                ]
              }
            ]
          },
          variations: [
            {
              id: '300005-var-1',
              name: 'Variation JS Condition',
              status: 'running',
              is_baseline: true,
              changes: [],
              key: '300005-var-1',
              traffic_allocation: 100.0
            }
          ]
        }
      ]
    }
  }
) as unknown as ConfigType;
const createContextForRuleDataProvider = (
  ruleDataProvider: Record<string, any>
): c => {
  const contextConfiguration = objectDeepMerge(ruleDataProviderConfiguration, {
    ruleDataProvider
  }) as unknown as ConfigType;
  const contextEventManager = new em(contextConfiguration);
  const contextApiManager = new am(contextConfiguration, {
    eventManager: contextEventManager
  });
  const contextBucketingManager = new bm(contextConfiguration);
  const contextRuleManager = new rm(contextConfiguration);
  const contextDataManager = new dm(contextConfiguration, {
    bucketingManager: contextBucketingManager,
    ruleManager: contextRuleManager,
    eventManager: contextEventManager,
    apiManager: contextApiManager
  });
  const contextExperienceManager = new exm(contextConfiguration, {
    dataManager: contextDataManager
  });
  const contextFeatureManager = new fm(contextConfiguration, {
    dataManager: contextDataManager
  });
  const contextSegmentsManager = new sm(contextConfiguration, {
    dataManager: contextDataManager,
    ruleManager: contextRuleManager
  });
  return new c(
    contextConfiguration,
    'RULEDATA-VISITOR',
    {
      eventManager: contextEventManager,
      experienceManager: contextExperienceManager,
      featureManager: contextFeatureManager,
      segmentsManager: contextSegmentsManager,
      dataManager: contextDataManager,
      apiManager: contextApiManager
    },
    {browser: 'chrome', country: 'US'}
  );
};

describe('Context tests', function () {
  const ruleDataProvider = {
    name: 'RuleData',
    getUrl: () => 'https://convert.com/',
    getCookie: () => 'utm_source=google',
    getCountry: () => 'US',
    getBrowserName: () => 'chrome',
    getJsCondition: () => true
  };
  describe('Context ruleDataProvider integration', function () {
    it('Should evaluate web-only rules with custom RuleData provider methods', function () {
      const context = createContextForRuleDataProvider(ruleDataProvider);

      const urlMatch = context.runExperience('ruledata-url', {
        enableTracking: false
      });
      const cookieMatch = context.runExperience('ruledata-cookie', {
        enableTracking: false
      });
      const geoMatch = context.runExperience('ruledata-geo', {
        enableTracking: false
      });
      const browserMatch = context.runExperience('ruledata-browser', {
        enableTracking: false
      });
      const jsConditionMatch = context.runExperience('ruledata-js-condition', {
        enableTracking: false
      });

      expect(urlMatch).to.be.an('object').that.has.property('experienceKey', 'ruledata-url');
      expect(cookieMatch).to.be.an('object').that.has.property(
        'experienceKey',
        'ruledata-cookie'
      );
      expect(geoMatch).to.be.an('object').that.has.property('experienceKey', 'ruledata-geo');
      expect(browserMatch).to.be.an('object').that.has.property(
        'experienceKey',
        'ruledata-browser'
      );
      expect(jsConditionMatch).to.be.an('object').that.has.property(
        'experienceKey',
        'ruledata-js-condition'
      );
    });
    it('Should ignore invalid ruleDataProvider marker and return no match', function () {
      const context = createContextForRuleDataProvider({
        name: 'wrong-name',
        getUrl: () => 'https://convert.com/'
      });
      expect(context.runExperience('ruledata-url', {enableTracking: false})).to.equal(
        null
      );
    });
    it('Should return no match without throwing when required getter is missing', function () {
      const context = createContextForRuleDataProvider({
        name: 'RuleData',
        getBrowserName: () => 'chrome'
      });
      expect(
        () =>
          expect(context.runExperience('ruledata-url', {enableTracking: false})).to.equal(
            null
          )
      ).to.not.throw();
    });
  });

  const visitorId = 'XXX';
  const featureId = '10025';
  it('Should expose Context', function () {
    assert.isDefined(c);
  });
  it('Imported entity should be a constructor of Context instance', function () {
    expect(c)
      .to.be.a('function')
      .that.has.property('name')
      .which.equal('Context');
  });
  it('Should successfully create new Context instance', async function () {
    const dataManager = new dm(configuration, {
      bucketingManager,
      ruleManager,
      eventManager,
      apiManager
    });
    const experienceManager = new exm(configuration, {dataManager});
    const featureManager = new fm(configuration, {dataManager});
    const segmentsManager = new sm(configuration, {dataManager, ruleManager});
    const context = new c(configuration, visitorId, {
      eventManager,
      experienceManager,
      featureManager,
      segmentsManager,
      dataManager,
      apiManager
    });
    expect(context)
      .to.be.an('object')
      .that.has.property('constructor')
      .that.has.property('name')
      .which.equal('Context');
  });
  describe('Test Context', function () {
    const visitorId = 'XXX';
    let dataManager,
      experienceManager,
      featureManager,
      segmentsManager,
      context,
      accountId,
      projectId,
      server;
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    before(function () {
      accountId = configuration?.data?.account_id;
      projectId = configuration?.data?.project?.id;
      dataManager = new dm(configuration, {
        bucketingManager,
        ruleManager,
        eventManager,
        apiManager
      });
      experienceManager = new exm(configuration, {dataManager});
      featureManager = new fm(configuration, {dataManager});
      segmentsManager = new sm(configuration, {dataManager, ruleManager});
      context = new c(
        configuration,
        visitorId,
        {
          eventManager,
          experienceManager,
          featureManager,
          segmentsManager,
          dataManager,
          apiManager
        },
        {browser: 'chrome', country: 'US'}
      );
    });
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    beforeEach(function () {
      server = http.createServer();
      server.listen(port);
    });
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    afterEach(function () {
      dataManager.reset();
      server.closeAllConnections();
      server.close();
    });
    it('Shoud successfully get variation from specific experience', function (done) {
      this.timeout(test_timeout);
      const experienceKey = 'test-experience-ab-fullstack-2';
      const variation = context.runExperience(experienceKey, {
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        }
      });
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', () => {
            expect(variation)
              .to.be.an('object')
              .that.have.keys(
                'experienceId',
                'experienceKey',
                'experienceName',
                'bucketingAllocation',
                'experienceType',
                'id',
                'key',
                'name',
                'status',
                'changes',
                'is_baseline',
                'traffic_allocation'
              );
            expect(variation.experienceKey).to.equal(experienceKey);
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end('{}');
      });
    });
    it('Shoud successfully get variations across all experiences', function (done) {
      this.timeout(test_timeout);
      getVariationsAcrossAllExperiences(
        {
          accountId,
          projectId,
          context,
          server
        },
        done
      );
    });
    it('Shoud successfully get a single feature and its status', function (done) {
      this.timeout(test_timeout);
      getSingleFeatureWithStatus(
        {
          accountId,
          projectId,
          featureId,
          context,
          server
        },
        done
      );
    });
    it('Shoud successfully get multiple features and its status', function (done) {
      this.timeout(test_timeout);
      getMultipleFeatureWithStatus(
        {
          accountId,
          projectId,
          context,
          server
        },
        done
      );
    });
    it('Shoud successfully get features and their statuses', function (done) {
      this.timeout(test_timeout);
      getFeaturesWithStatuses(
        {
          accountId,
          projectId,
          context,
          server
        },
        done
      );
    });
    it('Should trigger Conversion', function (done) {
      this.timeout(test_timeout);
      const goalKey = 'increase-engagement';
      const requestData = {
        source: 'js-sdk',
        enrichData: true,
        accountId,
        projectId,
        visitors: [
          {
            visitorId,
            events: [
              {
                eventType: 'conversion',
                data: {
                  goalId: '100215960'
                }
              },
              {
                eventType: 'conversion',
                data: {
                  goalId: '100215960',
                  goalData: [
                    {
                      key: 'amount',
                      value: 10.3
                    },
                    {
                      key: 'productsCount',
                      value: 2
                    }
                  ]
                }
              }
            ]
          }
        ]
      };
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          const body = [];
          request
            .on('data', (chunk) => {
              body.push(chunk);
            })
            .on('end', () => {
              const data = JSON.parse(Buffer.concat(body).toString());
              expect(data).to.be.an('object').that.deep.equal(requestData);
              done();
            });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end('{}');
      });
      context.trackConversion(goalKey, {
        ruleData: {
          action: 'buy'
        },
        conversionData: [
          {
            key: 'amount',
            value: 10.3
          },
          {
            key: 'productsCount',
            value: 2
          }
        ]
      });
    });
    it('Should fail to trigger Conversion if passing invalid goal data', function () {
      this.timeout(test_timeout);
      const goalKey = 'increase-engagement';
      const response = context.trackConversion(goalKey, {
        ruleData: {
          action: 'buy'
        },
        conversionData: [
          {
            key: 'amount',
            value: 10.3
          },
          {
            key: 'productsCount',
            value: 2
          }
        ]
      });
      expect(response).to.be.undefined;
    });

    it('Should fail to trigger Conversion if goal data is not an array', function () {
      const goalKey = 'increase-engagement';
      const response = context.trackConversion(goalKey, {
        ruleData: {
          action: 'buy'
        },
        conversionData: {
          amount: 10.3
        } as any
      });
      expect(response).to.be.undefined;
    });
    it('Should successfully set default segments', function () {
      const segments = {country: 'UK'};
      context.setDefaultSegments(segments);
      const localSegments = dataManager.getData(visitorId);
      expect(segments).to.deep.equal(localSegments?.segments);
    });
    it('Should successfully run custom segments', function () {
      const segmentKey = 'test-segments-1';
      const segmentId = '200299434';
      context.runCustomSegments(segmentKey, {
        ruleData: {
          enabled: true
        }
      });
      const {segments} = dataManager.getData(visitorId) || {};
      expect(segments)
        .to.be.an('object')
        .that.has.property('customSegments')
        .to.deep.equal([segmentId]);
    });
    it('Should successfully update visitor properties', function () {
      const properties = {weather: 'rainy'};
      context.updateVisitorProperties(visitorId, properties);
      const localSegments = dataManager.getData(visitorId);
      expect(properties).to.deep.equal(localSegments?.segments);
    });
    it('Should successfully get config entity', function () {
      const audienceKey = 'adv-audience';
      const audienceEntity = context.getConfigEntity(
        audienceKey,
        EntityType.AUDIENCE
      );
      const audience = configuration?.data?.audiences?.find?.(
        ({key}) => key === audienceKey
      );
      expect(audienceEntity).to.deep.equal(audience);
      const segmentKey = 'test-segments-1';
      const segmentEntity = context.getConfigEntity(
        segmentKey,
        EntityType.SEGMENT
      );
      const segment = configuration?.data?.segments?.find?.(
        ({key}) => key === segmentKey
      );
      expect(segmentEntity).to.deep.equal(segment);
      const featureKey = 'feature-2';
      const featureEntity = context.getConfigEntity(
        featureKey,
        EntityType.FEATURE
      );
      const feature = configuration?.data?.features?.find?.(
        ({key}) => key === featureKey
      );
      expect(featureEntity).to.deep.equal(feature);
      const goalKey = 'adv-goal-country-browser';
      const goalEntity = context.getConfigEntity(goalKey, EntityType.GOAL);
      const goal = configuration?.data?.goals?.find?.(
        ({key}) => key === goalKey
      );
      expect(goalEntity).to.deep.equal(goal);
      const experienceKey = 'test-experience-ab-fullstack-3';
      const experienceEntity = context.getConfigEntity(
        experienceKey,
        EntityType.EXPERIENCE
      );
      const experience = configuration?.data?.experiences?.find?.(
        ({key}) => key === experienceKey
      );
      expect(experienceEntity).to.deep.equal(experience);
      const variationKey = '100299461-variation-1';
      const variationEntity = context.getConfigEntity(
        variationKey,
        EntityType.VARIATION
      );
      const variation = experience?.variations?.find?.(
        ({key}) => key === variationKey
      );
      expect(variationEntity).to.deep.equal(variation);
    });
    it('Should successfully get config entity by id', function () {
      const audienceId = '100299433';
      const audienceEntity = context.getConfigEntityById(
        audienceId,
        EntityType.AUDIENCE
      );
      const audience = configuration?.data?.audiences?.find?.(
        ({id}) => id === audienceId
      );
      expect(audienceEntity).to.deep.equal(audience);
      const segmentId = '200299434';
      const segmentEntity = context.getConfigEntityById(
        segmentId,
        EntityType.SEGMENT
      );
      const segment = configuration?.data?.segments?.find?.(
        ({id}) => id === segmentId
      );
      expect(segmentEntity).to.deep.equal(segment);
      const featureEntity = context.getConfigEntityById(
        featureId,
        EntityType.FEATURE
      );
      const feature = configuration?.data?.features?.find?.(
        ({id}) => id === featureId
      );
      expect(featureEntity).to.deep.equal(feature);
      const goalId = '100215961';
      const goalEntity = context.getConfigEntityById(goalId, EntityType.GOAL);
      const goal = configuration?.data?.goals?.find?.(({id}) => id === goalId);
      expect(goalEntity).to.deep.equal(goal);
      const experienceId = '100218246';
      const experienceEntity = context.getConfigEntityById(
        experienceId,
        EntityType.EXPERIENCE
      );
      const experience = configuration?.data?.experiences?.find?.(
        ({id}) => id === experienceId
      );
      expect(experienceEntity).to.deep.equal(experience);
      const variationId = '100299461';
      const variationEntity = context.getConfigEntityById(
        variationId,
        EntityType.VARIATION
      );
      const variation = experience?.variations?.find?.(
        ({id}) => id === variationId
      );
      expect(variationEntity).to.deep.equal(variation);
    });
    it('Should successfully get visitor data', function () {
      const experienceKey = 'test-experience-ab-fullstack-2';
      const variation = context.runExperience(experienceKey, {
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        }
      });
      const data = context.getVisitorData();
      expect(data).to.deep.equal({
        bucketing: {[variation.experienceId]: variation.id}
      });
    });
  });
  describe('Test mixed experience type bucketing', function () {
    let dataManager, experienceManager, featureManager, segmentsManager, context;
    const mixedVisitorId = 'TYPE-100';
    const mixedEventManager = new em(mixedTypeConfiguration);
    const mixedApiManager = new am(mixedTypeConfiguration, {
      eventManager: mixedEventManager
    });
    const mixedRuleManager = new rm(mixedTypeConfiguration);
    const mixedBucketingManager = new bm(mixedTypeConfiguration);
    before(function () {
      dataManager = new dm(mixedTypeConfiguration, {
        bucketingManager: mixedBucketingManager,
        ruleManager: mixedRuleManager,
        eventManager: mixedEventManager,
        apiManager: mixedApiManager
      });
      experienceManager = new exm(mixedTypeConfiguration, {dataManager});
      featureManager = new fm(mixedTypeConfiguration, {dataManager});
      segmentsManager = new sm(mixedTypeConfiguration, {
        dataManager,
        ruleManager: mixedRuleManager
      });
      context = new c(
        mixedTypeConfiguration,
        mixedVisitorId,
        {
          eventManager: mixedEventManager,
          experienceManager,
          featureManager,
          segmentsManager,
          dataManager,
          apiManager: mixedApiManager
        },
        {browser: 'chrome', country: 'US'}
      );
    });
    afterEach(function () {
      dataManager.reset();
    });
    it('Should include experienceType in bucketed variation payload', function () {
      const fullstackVariation = context.runExperience(
        'test-experience-ab-fullstack-2',
        {
          locationProperties: {url: 'https://convert.com/'},
          visitorProperties: {
            varName3: 'something'
          },
          enableTracking: false
        }
      );
      const webVariation = context.runExperience(
        'test-experience-ab-web-1',
        {
          locationProperties: {url: 'https://convert.com/'},
          visitorProperties: {
            varName3: 'something'
          },
          enableTracking: false
        }
      );
      expect(fullstackVariation).to.have.property('experienceType', 'a/b_fullstack');
      expect(webVariation).to.have.property('experienceType', 'a/b');
    });
    it('Should return mixed web and fullstack variations without filtering', function () {
      const variations = context.runExperiences({
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        },
        enableTracking: false
      });
      expect(variations).to.have.length(3);
      expect(
        variations
          .map(({id}) => id)
          .includes('100299464')
      ).to.equal(true);
    });
    it('Should filter variations by provided experience type', function () {
      const webVariations = context.runExperiences({
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        },
        experienceTypes: ['a/b'],
        enableTracking: false
      });
      const fullstackVariations = context.runExperiences({
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        },
        experienceTypes: ['a/b_fullstack'],
        enableTracking: false
      });
      const unknownVariations = context.runExperiences({
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        },
        experienceTypes: ['split_url'],
        enableTracking: false
      });
      expect(webVariations).to.be.an('array').that.have.length(1);
      expect(
        webVariations.every(({experienceType}) => experienceType === 'a/b')
      ).to.equal(true);
      expect(fullstackVariations).to.be.an('array').that.have.length(2);
      expect(
        fullstackVariations.every(
          ({experienceType}) => experienceType === 'a/b_fullstack'
        )
      ).to.equal(true);
      expect(unknownVariations).to.be.an('array').that.have.length(0);
    });
    it('Should treat empty experienceTypes filter as no filtering', function () {
      const variations = context.runExperiences({
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        },
        experienceTypes: [],
        enableTracking: false
      });
      expect(variations).to.have.length(3);
    });
  });
  describe('Test Context bucketing contract', function () {
    let dataManager, experienceManager, featureManager, segmentsManager, testContext;
    const offlineVisitorId = 'OFFLINE-EXPERIENCE-BUCKETING';
    before(function () {
      dataManager = new dm(configuration, {
        bucketingManager,
        ruleManager,
        eventManager,
        apiManager
      });
      experienceManager = new exm(configuration, {dataManager});
      featureManager = new fm(configuration, {dataManager});
      segmentsManager = new sm(configuration, {dataManager, ruleManager});
      testContext = new c(
        configuration,
        offlineVisitorId,
        {
          eventManager,
          experienceManager,
          featureManager,
          segmentsManager,
          dataManager,
          apiManager
        },
        {browser: 'chrome', country: 'US'}
      );
    });
    afterEach(function () {
      dataManager.reset();
    });
    it('Should return stable bucketing payload for repeated runExperience calls', function () {
      const experienceKey = 'test-experience-ab-fullstack-2';
      const firstVariation = testContext.runExperience(experienceKey, {
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        },
        enableTracking: false
      });
      const secondVariation = testContext.runExperience(experienceKey, {
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        },
        enableTracking: false
      });
      expect(firstVariation).to.be.an('object');
      expect(secondVariation).to.be.an('object');
      expect(firstVariation).to.have.property('id');
      expect(secondVariation).to.have.property('id');
      expect(secondVariation.id).to.equal(firstVariation.id);
      expect(firstVariation).to.have.property(
        'experienceType',
        'a/b_fullstack'
      );
      expect(firstVariation)
        .to.have.property('bucketingAllocation')
        .that.is.a('number');
      expect(firstVariation)
        .to.have.property('traffic_allocation')
        .that.is.a('number');
      expect(testContext.getVisitorData())
        .to.have.property('bucketing')
        .to.deep.equal({[firstVariation.experienceId]: firstVariation.id});
    });
    it('Shoud fail to get variation when bucketing cannot be resolved', function () {
      const experienceKey = 'test-experience-ab-fullstack-4';
      const variation = testContext.runExperience(experienceKey, {
        locationProperties: {url: 'https://convert.com/'},
        visitorProperties: {
          varName3: 'something'
        },
        enableTracking: false
      });
      expect(variation).to.equal(BucketingError.VARIAION_NOT_DECIDED);
    });
  });
  describe('Test variation rendering', function () {
    let dataManager,
      experienceManager,
      featureManager,
      segmentsManager,
      context,
      events;
    let originalDocument;
    let originalWindow;

    const createBrowserStub = (): {
      document: any;
      events: Array<string>;
    } => {
      const events: Array<string> = [];
      const assignAppend = (element, child: any) => {
        if (
          child &&
          Object.prototype.hasOwnProperty.call(child, 'text') &&
          child?.text !== undefined
        ) {
          element.textContent = child.text;
        }
      };
      const eventLabel = (node: any): string => {
        if (node?.tagName === 'style') return 'css';
        if (node?.tagName === 'script') return 'js';
        return node?.tagName;
      };
      const container = {
        appendChild: (node: any) => {
          const text = node?.textContent || '';
          events.push(`${eventLabel(node)}:${text}`);
          return node;
        }
      };
      return {
        document: {
          head: container,
          body: container,
          documentElement: container,
          createTextNode: (value: string) => ({text: value}),
          createElement: (tagName: string) => {
            const element = {
              tagName,
              type: '',
              textContent: '',
              appendChild: (node: any) => assignAppend(element, node),
              setAttribute: () => {}
            };
            return element;
          },
          getElementsByTagName: () => [container]
        },
        events
      };
    };

    const getContext = () => {
      dataManager = new dm(configuration, {
        bucketingManager,
        ruleManager,
        eventManager,
        apiManager
      });
      experienceManager = new exm(configuration, {dataManager});
      featureManager = new fm(configuration, {dataManager});
      segmentsManager = new sm(configuration, {
        dataManager,
        ruleManager
      });
      return new c(
        configuration,
        'RUN-VARIATION-VISITOR',
        {
          eventManager,
          experienceManager,
          featureManager,
          segmentsManager,
          dataManager,
          apiManager
        },
        {browser: 'chrome', country: 'US'}
      );
    };

    beforeEach(function () {
      const setup = createBrowserStub();
      originalDocument = (global as any).document;
      originalWindow = (global as any).window;
      events = setup.events;
      (global as any).document = setup.document;
      (global as any).window = setup.document;
      context = getContext();
    });

    afterEach(function () {
      (global as any).document = originalDocument;
      (global as any).window = originalWindow;
    });

    it('Should execute global then per-change web payload in expected order', function () {
      (global as any).window.__webVariationLog = [];
      const variation = {
        id: '100500100',
        experienceKey: 'web-experience',
        changes: [
          {
            id: 10,
            type: 'defaultCode',
            data: {
              css: '.global .item { color: red; }',
              js: 'window.__webVariationLog.push("defaultCode-js");',
              custom_js: 'window.__webVariationLog.push("defaultCode-custom");'
            }
          },
          {
            id: 11,
            type: 'defaultRedirect',
            data: {}
          },
          {
            id: 12,
            type: 'defaultCodeMultipage',
            data: {
              css: '.page .item { color: blue; }',
              js: 'window.__webVariationLog.push("defaultCodeMultipage-js");',
              custom_js:
                'window.__webVariationLog.push("defaultCodeMultipage-custom");'
            }
          },
          {
            id: 13,
            type: 'customCode',
            data: {
              css: '.custom .item { color: green; }',
              js: 'window.__webVariationLog.push("customCode-js");'
            }
          }
        ]
      };
      const experience = {
        key: 'web-experience',
        global_js: 'window.__webVariationLog.push("global-js");',
        global_css: '.global {display:block;}'
      } as any;

      context.runVariation(variation as any, {experience});
      expect(events[0]).to.include('css');
      expect(events[0]).to.include('global {display:block;}');
      expect(events[1]).to.include('css:.global .item { color: red; }');
      expect(events[2]).to.include('css:.page .item { color: blue; }');
      expect(events[3]).to.include('css:.custom .item { color: green; }');
      expect(events).to.have.length(4);
      expect((global as any).window.__webVariationLog).to.deep.equal([
        'global-js',
        'defaultCode-js',
        'defaultCode-custom',
        'defaultCodeMultipage-js',
        'defaultCodeMultipage-custom',
        'customCode-js'
      ]);
    });

    it('Should skip unsupported web variation change types', function () {
      const variation = {
        id: '100500102',
        experienceKey: 'web-experience-nonweb',
        changes: [
          {
            id: 21,
            type: 'fullStackFeature',
            data: {
              css: '.should-never-be-added { color: purple; }',
              js: 'window.__webVariationLog = "fullStackFeature-js";'
            }
          },
          {
            id: 22,
            type: 'defaultRedirect',
            data: {
              css: '.redirect { color: yellow; }',
              js: 'window.__webVariationLog = "defaultRedirect-js";'
            }
          },
          {
            id: 23,
            type: 'defaultCode',
            data: {
              css: '.still-applied { color: black; }'
            }
          }
        ]
      };
      const experience = {
        key: 'web-experience-nonweb',
        global_css: '.global {display:block;}'
      } as any;

      context.runVariation(variation as any, {experience});
      expect(events[0]).to.include('css:.global {display:block;}');
      expect(events[1]).to.include('css:.still-applied { color: black; }');
      expect(events).to.have.length(2);
    });

    it('Should keep variation and change changes idempotent across repeated rendering calls', function () {
      const variation = {
        id: '100500101',
        experienceKey: 'web-experience-repeat',
        changes: [
          {
            id: 21,
            type: 'defaultCode',
            data: {
              css: '.global .item { color: red; }'
            }
          }
        ]
      };
      const experience = {
        key: 'web-experience-repeat',
        global_css: '.global {display:block;}'
      } as any;

      context.runVariation(variation as any, {experience});
      context.runVariation(variation as any, {experience});
      expect(events).to.have.length(2);
      expect(events[0]).to.include('css:.global {display:block;}');
      expect(events[1]).to.include('css:.global .item { color: red; }');
    });

    it('Should skip global rendering when document API is unavailable', function () {
      const variation = {
        id: '100500104',
        experienceKey: 'web-missing-document',
        changes: [
          {
            id: 31,
            type: 'defaultCode',
            data: {
              css: '.not-added { color: red; }'
            }
          }
        ]
      };
      const experience = {
        key: 'web-missing-document',
        global_css: '.global {display:block;}'
      } as any;

      (global as any).document = undefined;
      context.runVariation(variation as any, {experience});
      expect(events).to.have.length(0);
    });

    it('Should skip global rendering when no append target exists', function () {
      const variation = {
        id: '100500105',
        experienceKey: 'web-no-appender',
        changes: [
          {
            id: 32,
            type: 'defaultCode',
            data: {
              css: '.not-added { color: red; }'
            }
          }
        ]
      };
      const experience = {
        key: 'web-no-appender',
        global_css: '.global {display:block;}'
      } as any;

      (global as any).document = {
        createElement: (tagName: string) => ({tagName, type: '', textContent: ''}),
        createTextNode: (value: string) => ({text: value}),
        getElementsByTagName: () => []
      } as any;

      context.runVariation(variation as any, {experience});
      expect(events).to.have.length(0);
    });

    it('Should skip rendering when append target does not support appendChild', function () {
      const variation = {
        id: '100500106',
        experienceKey: 'web-no-append-child',
        changes: [
          {
            id: 33,
            type: 'defaultCode',
            data: {
              css: '.not-added { color: red; }'
            }
          }
        ]
      };
      const experience = {
        key: 'web-no-append-child',
        global_css: '.global {display:block;}'
      } as any;
      const appender = {};

      (global as any).document = {
        head: appender,
        body: appender,
        documentElement: appender,
        createElement: (tagName: string) => ({tagName, type: '', textContent: ''}),
        createTextNode: (value: string) => ({text: value}),
        getElementsByTagName: () => [appender]
      } as any;

      context.runVariation(variation as any, {experience});
      expect(events).to.have.length(0);
    });

    it('Should fallback to style.textContent when createTextNode is unavailable', function () {
      const variation = {
        id: '100500107',
        experienceKey: 'web-style-fallback',
        changes: [
          {
            id: 34,
            type: 'defaultCode',
            data: {
              css: '.fallback-item { color: green; }'
            }
          }
        ]
      };
      const experience = {
        key: 'web-style-fallback',
        global_css: '.global {display:block;}'
      } as any;
      const rendered: string[] = [];
      const createElement = (tagName: string) => {
        if (tagName === 'style') {
          return {tagName, type: '', textContent: ''};
        }
        const element: any = {tagName, type: '', textContent: ''};
        element.appendChild = (node: any) => {
          if (
            node &&
            Object.prototype.hasOwnProperty.call(node, 'text') &&
            node?.text !== undefined
          ) {
            element.textContent = node.text;
          }
        };
        return element;
      };
      const appender = {
        appendChild: (node: any) => {
          rendered.push(`${node?.tagName}:${node?.textContent || ''}`);
        }
      };
      (global as any).document = {
        head: appender,
        body: appender,
        documentElement: appender,
        createElement,
        getElementsByTagName: () => [appender]
      } as any;

      context.runVariation(variation as any, {experience});
      expect(rendered).to.include('style:.global {display:block;}');
      expect(rendered).to.include('style:.fallback-item { color: green; }');
      expect(rendered).to.have.length(2);
    });

    it('Should continue rendering when CSS injection fails but JS injection still runs', function () {
      (global as any).window.__webVariationLog = [];
      const variation = {
        id: '100500108',
        experienceKey: 'web-css-error',
        changes: []
      };
      const experience = {
        key: 'web-css-error',
        global_js: 'window.__webVariationLog.push("global-css-error");'
      } as any;
      const rendered: string[] = [];
      const appender = {
        appendChild: (node: any) => {
          rendered.push(`${node?.tagName}:${node?.textContent || ''}`);
        }
      };
      (global as any).document = {
        head: appender,
        body: appender,
        documentElement: appender,
        createTextNode: (value: string) => ({text: value}),
        createElement: (tagName: string) => {
          if (tagName === 'style') {
            throw new Error('style injection failed');
          }
          return {tagName, type: '', textContent: ''};
        },
        getElementsByTagName: () => [appender]
      } as any;

      context.runVariation(variation as any, {experience});
      expect(rendered).to.have.length(0);
      expect((global as any).window.__webVariationLog).to.deep.equal([
        'global-css-error'
      ]);
    });

    it('Should continue rendering when JS injection fails', function () {
      const variation = {
        id: '100500109',
        experienceKey: 'web-js-error',
        changes: []
      };
      const experience = {
        key: 'web-js-error',
        global_css: '.global {display:block;}'
      } as any;
      const rendered: string[] = [];
      const appender = {
        appendChild: (node: any) => {
          rendered.push(`${node?.tagName}:${node?.textContent || ''}`);
        }
      };
      (global as any).document = {
        head: appender,
        body: appender,
        documentElement: appender,
        createTextNode: (value: string) => ({text: value}),
        createElement: (tagName: string) => {
          if (tagName === 'script') {
            throw new Error('script injection failed');
          }
          const element: any = {tagName, type: '', textContent: ''};
          element.appendChild = (node: any) => {
            if (
              node &&
              Object.prototype.hasOwnProperty.call(node, 'text') &&
              node?.text !== undefined
            ) {
              element.textContent = node.text;
            }
          };
          return element;
        },
        getElementsByTagName: () => [appender]
      } as any;

      context.runVariation(variation as any, {experience});
      expect(rendered).to.have.length(1);
      expect(rendered[0]).to.equal('style:.global {display:block;}');
    });

    it('Should execute changes when no change id is provided', function () {
      const variation = {
        changes: [
          {
            type: 'defaultCode',
            data: {
              css: '.no-id-item { color: orange; }'
            }
          }
        ]
      };
      const experience = {
        key: 'web-no-id-change',
        global_css: '.global {display:block;}'
      } as any;

      context.runVariation(variation as any, {experience});
      expect(events[0]).to.include('css:.global {display:block;}');
      expect(events[1]).to.include('css:.no-id-item { color: orange; }');
      expect(events).to.have.length(2);
    });
  });
  describe('Test invalid visitor', function () {
    let dataManager,
      experienceManager,
      featureManager,
      segmentsManager,
      context;
    // eslint-disable-next-line mocha/no-hooks-for-single-case
    before(function () {
      dataManager = new dm(configuration, {
        bucketingManager,
        ruleManager,
        eventManager,
        apiManager
      });
      experienceManager = new exm(configuration, {dataManager});
      featureManager = new fm(configuration, {dataManager});
      segmentsManager = new sm(configuration, {dataManager, ruleManager});
      context = new c(configuration, null, {
        eventManager,
        experienceManager,
        featureManager,
        segmentsManager,
        dataManager,
        apiManager
      });
    });
    it('Shoud fail to get variation from specific experience if no visitor is set', function () {
      const experienceKey = 'test-experience-ab-fullstack-2';
      const variation = context.runExperience(experienceKey);
      expect(variation).to.be.undefined;
    });
    it('Shoud fail to get variations across all experiences if no visitor is set', function () {
      const variations = context.runExperiences();
      expect(variations).to.be.undefined;
    });
    it('Shoud fail to get feature and its status if no visitor is set', function () {
      const featureKey = 'feature-1';
      const features = context.runFeature(featureKey);
      expect(features).to.be.undefined;
    });
    it('Shoud fail to get features and their statuses if no visitor is set', function () {
      const features = context.runFeatures();
      expect(features).to.be.undefined;
    });
    it('Should fail to trigger Conversion if no visitor is set', function () {
      const goalKey = 'increase-engagement';
      const output = context.trackConversion(goalKey);
      expect(output).to.be.undefined;
    });
    it('Should fail to set custom segments if no visitor is set', function () {
      const segmentKey = 'test-segments-1';
      const output = context.setCustomSegments(segmentKey);
      expect(output).to.be.undefined;
    });
  });
  describe('Test Context branch coverage', function () {
    const createMockContext = ({
      experienceManager,
      featureManager,
      segmentsManager,
      dataManager,
      apiManager
    }: {
      experienceManager?: any;
      featureManager?: any;
      segmentsManager?: any;
      dataManager?: any;
      apiManager?: any;
    } = {}): c => {
      return new c(
        configuration,
        'BRANCH-COVERAGE-VISITOR',
        {
          eventManager,
          experienceManager: experienceManager || {selectVariation: () => null},
          featureManager:
            featureManager || {
              runFeature: () => null,
              runFeatures: () => []
            },
          segmentsManager:
            segmentsManager || {
              getSegments: () => ({}),
              selectCustomSegments: () => null
            },
          dataManager: dataManager || {getData: () => ({})},
          apiManager: apiManager || {releaseQueue: () => Promise.resolve()}
        } as any
      );
    };

    it('Should return rule error from runExperience', function () {
      const context = createMockContext({
        experienceManager: {
          selectVariation: () => RuleError.NO_DATA_FOUND
        } as any
      });
      const output = context.runExperience('any-experience');
      expect(output).to.equal(RuleError.NO_DATA_FOUND);
    });

    it('Should return bucketing error from runExperience', function () {
      const context = createMockContext({
        experienceManager: {
          selectVariation: () => BucketingError.VARIAION_NOT_DECIDED
        } as any
      });
      const output = context.runExperience('any-experience');
      expect(output).to.equal(BucketingError.VARIAION_NOT_DECIDED);
    });

    it('Should return rule errors from runExperiences', function () {
      const context = createMockContext({
        experienceManager: {
          selectVariations: () => [RuleError.NO_DATA_FOUND, RuleError.NEED_MORE_DATA]
        } as any
      });
      const output = context.runExperiences();
      expect(output).to.deep.equal([
        RuleError.NO_DATA_FOUND,
        RuleError.NEED_MORE_DATA
      ]);
    });

    it('Should return bucketing errors from runExperiences', function () {
      const context = createMockContext({
        experienceManager: {
          selectVariations: () => [BucketingError.VARIAION_NOT_DECIDED]
        } as any
      });
      const output = context.runExperiences();
      expect(output).to.deep.equal([BucketingError.VARIAION_NOT_DECIDED]);
    });

    it('Should return rule error from runFeature', function () {
      const context = createMockContext({
        featureManager: {
          runFeature: () => RuleError.NO_DATA_FOUND
        } as any
      });
      const output = context.runFeature('feature-1');
      expect(output).to.equal(RuleError.NO_DATA_FOUND);
    });

    it('Should return rule errors from runFeatures', function () {
      const context = createMockContext({
        featureManager: {
          runFeatures: () => [RuleError.NO_DATA_FOUND]
        } as any
      });
      const output = context.runFeatures();
      expect(output).to.deep.equal([RuleError.NO_DATA_FOUND]);
    });

    it('Should return rule error from trackConversion', function () {
      const context = createMockContext({
        segmentsManager: {
          getSegments: () => ({})
        } as any,
        dataManager: {
          getData: () => ({}),
          convert: () => RuleError.NEED_MORE_DATA
        } as any
      });
      const output = context.trackConversion('increase-engagement');
      expect(output).to.equal(RuleError.NEED_MORE_DATA);
    });

    it('Should return rule error from setCustomSegments', function () {
      const context = createMockContext({
        segmentsManager: {
          getSegments: () => ({}),
          selectCustomSegments: () => RuleError.NO_DATA_FOUND
        } as any
      });
      const output = context.runCustomSegments(['feature-segment']);
      expect(output).to.equal(RuleError.NO_DATA_FOUND);
    });

    it('Should release pending queues only through API when no dataStoreManager is available', async function () {
      let apiReleaseCalls = 0;
      let dataStoreReleaseCalls = 0;
      const context = createMockContext({
        dataManager: {
          getData: () => ({}),
          dataStoreManager: undefined
        } as any,
        apiManager: {
          releaseQueue: () => {
            apiReleaseCalls += 1;
            return Promise.resolve();
          }
        } as any
      });

      await context.releaseQueues('context-branch');
      expect(apiReleaseCalls).to.equal(1);
      expect(dataStoreReleaseCalls).to.equal(0);
    });

    it('Should release pending queues through both dataStoreManager and API', async function () {
      let apiReleaseCalls = 0;
      let dataStoreReleaseCalls = 0;
      const context = createMockContext({
        dataManager: {
          getData: () => ({}),
          dataStoreManager: {
            releaseQueue: () => {
              dataStoreReleaseCalls += 1;
              return Promise.resolve();
            }
          }
        } as any,
        apiManager: {
          releaseQueue: () => {
            apiReleaseCalls += 1;
            return Promise.resolve();
          }
        } as any
      });

      await context.releaseQueues('context-branch');
      expect(apiReleaseCalls).to.equal(1);
      expect(dataStoreReleaseCalls).to.equal(1);
    });
  });
});
