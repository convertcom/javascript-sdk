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
import {FeatureManager as fm} from '../src/feature-manager';
import testConfig from './test-config.json';
import {Config as ConfigType} from '@convertcom/js-sdk-types';
import {objectDeepMerge} from '@convertcom/js-sdk-utils';
import {defaultConfig} from '../src/config/default';
import {FeatureStatus, RuleError} from '@convertcom/js-sdk-enums';

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

describe('FeatureManager tests', function () {
  const visitorId = 'XXX';
  let dataManager, featureManager, accountId, projectId, server;
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
    featureManager = new fm(configuration, {dataManager});
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
  it('Should expose FeatureManager', function () {
    assert.isDefined(fm);
  });
  it('Imported entity should be a constructor of FeatureManager instance', function () {
    expect(fm)
      .to.be.a('function')
      .that.has.property('name')
      .which.equal('FeatureManager');
  });
  it('Should successfully create new FeatureManager instance', async function () {
    expect(featureManager)
      .to.be.an('object')
      .that.has.property('constructor')
      .that.has.property('name')
      .which.equal('FeatureManager');
  });
  describe('Test ConfigFeature Manager', function () {
    it('Shoud successfully get a list of all entities', function () {
      const entities = featureManager.getList();
      expect(entities)
        .to.be.an('array')
        .that.has.length(3)
        .to.deep.equal(configuration?.data?.features);
    });
    it('Shoud successfully get a list of all entities as object of entities grouped by identity field', function () {
      const field = 'id';
      const entities = featureManager.getListAsObject(field);
      const featuresList = Object.fromEntries(
        configuration?.data?.features?.map((feature) => [
          feature?.[field],
          feature
        ]) || []
      );
      expect(entities).to.be.an('object').that.deep.equal(featuresList);
    });
    it('Shoud successfully get the entity by key', function () {
      const featureKey = 'feature-1';
      const featureId = '10024';
      const entity = featureManager.getFeature(featureKey);
      expect(entity)
        .to.be.an('object')
        .that.has.property('id')
        .to.equal(featureId);
    });
    it('Shoud successfully get the entity by id', function () {
      const featureKey = 'feature-1';
      const featureId = '10024';
      const entity = featureManager.getFeatureById(featureId);
      expect(entity)
        .to.be.an('object')
        .that.has.property('key')
        .to.equal(featureKey);
    });
    it('Shoud successfully specific entities by array of keys', function () {
      const featureKeys = ['feature-1', 'feature-2', 'not-attached-feature-3'];
      const entities = featureManager.getFeatures(featureKeys);
      expect(entities)
        .to.be.an('array')
        .to.deep.equal(configuration?.data?.features);
    });
    it('Shoud successfully get a specific variable type defined in a specific feature', function () {
      const featureKey = 'feature-1';
      const variableName = 'enabled';
      const variableType = 'boolean';
      const type = featureManager.getFeatureVariableType(
        featureKey,
        variableName
      );
      expect(type).to.equal(variableType);
    });
    it('Shoud successfully get specific variable type defined in a specific feature by id', function () {
      const featureId = '10024';
      const variableName = 'enabled';
      const variableType = 'boolean';
      const type = featureManager.getFeatureVariableTypeById(
        featureId,
        variableName
      );
      expect(type).to.equal(variableType);
    });
    it('Shoud successfully check that feature is declared', function () {
      const featureKey = 'feature-1';
      const check = featureManager.isFeatureDeclared(featureKey);
      expect(check).to.equal(true);
    });
    it('Shoud successfully get feature and its status', function (done) {
      this.timeout(test_timeout);
      const featureKey = 'feature-1';
      const featureIds = ['10024', '10025'];
      const features = featureManager.runFeature(visitorId, featureKey, {
        visitorProperties: {
          varName3: 'something'
        },
        locationProperties: {url: 'https://convert.com/'}
      });
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', () => {
            expect(features).to.be.an('array').that.have.length(2);
            const selectedFeatures = features.map(({id}) => id);
            expect(featureIds).to.include.deep.members(selectedFeatures);
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end('{}');
      });
    });
    it('Shoud successfully check is feature enabled', function (done) {
      this.timeout(test_timeout);
      const featureKey = 'feature-1';
      const enabled = featureManager.isFeatureEnabled(visitorId, featureKey, {
        visitorProperties: {
          varName3: 'something'
        },
        locationProperties: {url: 'https://convert.com/'}
      });
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', () => {
            expect(enabled).to.equal(true);
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end('{}');
      });
    });
    it('Shoud successfully get feature and its status by id', function (done) {
      this.timeout(test_timeout);
      const featureId = '10024';
      const featureIds = ['10024', '10025'];
      const features = featureManager.runFeatureById(visitorId, featureId, {
        visitorProperties: {
          varName3: 'something'
        },
        locationProperties: {url: 'https://convert.com/'}
      });
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', () => {
            expect(features).to.be.an('array').that.have.length(2);
            const selectedFeatures = features.map(({id}) => id);
            expect(featureIds).to.include.deep.members(selectedFeatures);
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end('{}');
      });
    });
    it('Shoud successfully get features and their statuses', function (done) {
      this.timeout(test_timeout);
      const filterByFeatures = [
        'feature-1',
        'feature-2',
        'not-attached-feature-3'
      ];
      const filterByExperiences = [
        'test-experience-ab-fullstack-2',
        'test-experience-ab-fullstack-3'
      ];
      const featureIds = ['10024', '10025', '10026'];
      const features = featureManager.runFeatures(
        visitorId,
        {
          visitorProperties: {
            varName3: 'something'
          },
          locationProperties: {url: 'https://convert.com/'},
          updateVisitorProperties: false,
          typeCasting: true
        },
        {
          features: filterByFeatures,
          experiences: filterByExperiences
        }
      );
      server.on('request', (request, res) => {
        if (request.url.startsWith(`/track/${accountId}/${projectId}`)) {
          request.on('end', () => {
            expect(features).to.be.an('array').that.have.length(3);
            const selectedFeatures = features.map(({id}) => id);
            expect(featureIds).to.include.deep.members(selectedFeatures);
            done();
          });
        }
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end('{}');
      });
    });
  it('Convert value type', function () {
    let value = featureManager.castType('123', 'integer');
    expect(typeof value).to.equal('number');
    value = featureManager.castType(123, 'string');
    expect(typeof value).to.equal('string');
    value = featureManager.castType('1.23', 'float');
    expect(typeof value).to.equal('number');
    value = featureManager.castType('false', 'boolean');
    expect(typeof value).to.equal('boolean');
  });
  });

  describe('Test FeatureManager branch coverage', function () {
    it('Should return null when feature variables are not provided for type detection', function () {
      const featureWithoutVariables = {
        id: '90001',
        name: 'Feature without variables',
        key: 'feature-without-variables'
      };
      const customFeatureManager = new fm(null as any, {
        dataManager: {
          getEntity: () => featureWithoutVariables,
          getEntityById: () => featureWithoutVariables,
          getEntitiesListObject: () => ({
            'feature-without-variables': featureWithoutVariables
          })
        } as any
      });

      expect(customFeatureManager.getFeatureVariableType('feature-without-variables', 'missing')).to.be.null;
      expect(customFeatureManager.getFeatureVariableTypeById('90001', 'missing')).to.be.null;
    });

    it('Should return disabled feature when feature is declared but no bucketed variation is returned', function () {
      const declaredFeature = {id: '90002', key: 'feature-declared-no-variation', name: 'Feature disabled'};
      const featureManagerForNoVariation = new fm(null as any, {
        dataManager: {
          getEntity: (key: string) =>
            key === declaredFeature.key ? declaredFeature : null,
          getEntityById: () => declaredFeature,
          getListAsObject: () => ({[declaredFeature.id]: declaredFeature}),
          getEntitiesList: () => [{key: 'test-experience-branch-no-variation'}],
        getEntities: (keys: Array<string>) =>
            keys.map((key) => ({key, id: `id-${key}`})),
          getEntitiesByIds: (ids: Array<string>) =>
            ids.map((id) => ({id, key: `id-${id}`})),
          getEntitiesListObject: () => ({
            [declaredFeature.id]: declaredFeature
          }),
          getBucketing: () => ({
            experienceId: 'e1',
            experienceName: 'Exp',
            experienceKey: 'test-experience-branch-no-variation',
            changes: [{type: 'defaultCode', data: {}}]
          })
        } as any
      });

      const output = featureManagerForNoVariation.runFeature('feature-visitor-branch', declaredFeature.key, {});
      expect(output).to.deep.equal({
        id: declaredFeature.id,
        name: declaredFeature.name,
        key: declaredFeature.key,
        status: FeatureStatus.DISABLED
      });
    });

    it('Should return disabled feature if feature is not declared', function () {
      expect(featureManager.runFeature('BRANCH-VISITOR', 'feature-does-not-exist', {})).to.deep.equal({
        key: 'feature-does-not-exist',
        status: FeatureStatus.DISABLED
      });
    });

    it('Should return false if the feature is not declared', function () {
      expect(featureManager.isFeatureEnabled('BRANCH-VISITOR', 'feature-missing')).to.equal(
        false
      );
    });

    it('Should return disabled feature when runFeatureById is not declared', function () {
      expect(
        featureManager.runFeatureById('feature-visitor', 'feature-id-missing', {})
      ).to.deep.equal({
        id: 'feature-id-missing',
        status: FeatureStatus.DISABLED
      });
    });

    it('Should return a single feature from runFeatureById when only one match is found', function () {
      const declaredFeature = {
        id: '90003',
        key: 'feature-single-match',
        name: 'Single Match Feature',
        variables: [{key: 'enabled', type: 'boolean'}]
      };

      const managerWithSingleMatch = new fm(null as any, {
        dataManager: {
          getEntityById: (id: string) =>
            id === declaredFeature.id ? declaredFeature : null,
          getListAsObject: () => ({[declaredFeature.id]: declaredFeature}),
          getEntitiesList: () => [{key: 'test-experience-single'}],
          getEntities: () => [{key: 'test-experience-single', id: 'e1'}],
          getEntitiesByIds: (ids: Array<string> = []) =>
            ids.map((id) => ({id, key: `exp-${id}`})),
          getEntitiesListObject: () => ({
            [declaredFeature.id]: declaredFeature
          }),
          getBucketing: () => ({
            experienceId: 'e1',
            experienceName: 'Exp',
            experienceKey: 'test-experience-single',
            changes: [
              {
                type: 'fullStackFeature',
                data: {
                  feature_id: declaredFeature.id,
                  variables_data: {enabled: 'true'}
                }
              }
            ]
          })
        } as any
      });

      const output = managerWithSingleMatch.runFeatureById(
        'feature-visitor',
        declaredFeature.id,
        {} as any
      ) as any;
      expect(output).to.have.property('key', declaredFeature.key);
      expect(output).to.have.property('status', FeatureStatus.ENABLED);
    });

    it('Should return rule errors when runFeatures detects bucketing errors', function () {
      const managerWithErrors = new fm(null as any, {
        dataManager: {
          getListAsObject: () => ({}),
          getEntitiesList: () => [{key: 'test-experience-errors'}],
          getEntitiesListObject: () => ({}),
          getBucketing: () => RuleError.NO_DATA_FOUND
        } as any
      });

      const output = managerWithErrors.runFeatures('feature-visitor', {} as any);
      expect(output).to.deep.equal([RuleError.NO_DATA_FOUND]);
    });

    it('Should skip unsupported changes, missing feature ids, and missing feature variable type definitions', function () {
      const declaredFeatures = {
        f4: {
          id: 'f4',
          key: 'feature-with-variables',
          name: 'Feature with variable',
          variables: [{key: 'enabled', type: 'boolean'}]
        },
        f5: {
          id: 'f5',
          key: 'feature-without-variable-def',
          name: 'Feature without variable definition'
        }
      };

      const managerWithMixedChanges = new fm(null as any, {
        dataManager: {
          getListAsObject: () => ({
            f4: declaredFeatures.f4,
            f5: declaredFeatures.f5
          }),
          getEntitiesListObject: () => ({
            f4: declaredFeatures.f4,
            f5: declaredFeatures.f5
          }),
          getEntitiesList: () => [{key: 'test-experience-mixed'}],
          getBucketing: () => ({
            experienceId: 'e1',
            experienceName: 'Exp',
            experienceKey: 'test-experience-mixed',
            changes: [
              {type: 'defaultCode', data: {value: 1}},
              {
                type: 'fullStackFeature',
                data: {
                  feature_id: declaredFeatures.f4.id
                }
              },
              {
                type: 'fullStackFeature',
                data: {
                  feature_id: declaredFeatures.f5.id,
                  variables_data: {missingVariable: 'true'}
                }
              },
              {
                type: 'fullStackFeature',
                data: {}
              }
            ]
          })
        } as any
      });

      const output = managerWithMixedChanges.runFeatures(
        'feature-visitor',
        {} as any
      ) as any[];
      expect(output).to.be.an('array');
      expect(output).to.have.length(2);

      const featureFromChanges = output.find((item) => item.id === declaredFeatures.f4.id) as any;
      expect(featureFromChanges).to.have.property('status', FeatureStatus.ENABLED);
      expect(featureFromChanges).to.have.property('key', declaredFeatures.f4.key);
      expect(featureFromChanges).to.have.property('name', declaredFeatures.f4.name);
      expect(featureFromChanges).to.have.property('experienceId', 'e1');
      expect(featureFromChanges).to.have.property('experienceName', 'Exp');
      expect(featureFromChanges).to.have.property('experienceKey', 'test-experience-mixed');

      const featureWithMissingVariables = output.find(
        (item) => item.id === declaredFeatures.f5.id
      ) as any;
      expect(featureWithMissingVariables).to.have.property('status', FeatureStatus.ENABLED);
      expect(featureWithMissingVariables).to.have.property('key', declaredFeatures.f5.key);
      expect(featureWithMissingVariables).to.have.property('name', declaredFeatures.f5.name);
      expect(featureWithMissingVariables).to.have.property('variables');
      expect(featureWithMissingVariables.variables).to.deep.equal({
        missingVariable: 'true'
      });
    });
  });
});
