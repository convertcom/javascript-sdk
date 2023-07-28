/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {DataManagerInterface} from '@convertcom/js-sdk-data';
import {FeatureManagerInterface} from './interfaces/feature-manager';
import {LogManagerInterface} from '@convertcom/js-sdk-logger';

import {
  Config,
  Id,
  Feature,
  BucketedFeature,
  IdentityField,
  VariableType,
  Experience,
  FullStackFeatureChange
} from '@convertcom/js-sdk-types';
import {
  MESSAGES,
  FeatureStatus,
  RuleError,
  VariationChangeType
} from '@convertcom/js-sdk-enums';

import {castType, arrayNotEmpty} from '@convertcom/js-sdk-utils';
import {BucketedVariation} from '@convertcom/js-sdk-types';

/**
 * Provides features specific logic
 * @category Modules
 * @constructor
 * @implements {FeatureManagerInterface}
 */
export class FeatureManager implements FeatureManagerInterface {
  private _dataManager: DataManagerInterface;
  private _loggerManager: LogManagerInterface | null;

  /**
   * @param config
   * @param {Object} dependencies
   * @param {DataManagerInterface} dependencies.dataManager
   * @param {LogManagerInterface=} dependencies.loggerManager
   */
  constructor(
    config: Config,
    {
      dataManager,
      loggerManager
    }: {
      dataManager: DataManagerInterface;
      loggerManager?: LogManagerInterface;
    }
  ) {
    this._dataManager = dataManager;
    this._loggerManager = loggerManager;
    this._loggerManager?.trace?.(MESSAGES.FEATURE_CONSTRUCTOR);
  }

  /**
   * Get a list of all entities
   * @return {Array<Feature>} Features list
   */
  getList(): Array<Feature> {
    return this._dataManager.getEntitiesList('features') as Array<Feature>;
  }

  /**
   * Get a list of all entities as object of entities grouped by identity field
   * @param {IdentityField=} field A field to group entities defaults to `id`
   * @return {Record<string, Feature>} Features list
   */
  getListAsObject(field: IdentityField = 'id'): Record<string, Feature> {
    return this._dataManager.getEntitiesListObject('features', field) as Record<
      string,
      Feature
    >;
  }

  /**
   * Get the entity by key
   * @param {string} key
   * @return {Feature}
   */
  getFeature(key: string): Feature {
    return this._dataManager.getEntity(key, 'features') as Feature;
  }

  /**
   * Get the entity by id
   * @param {Id} id
   * @return {Feature}
   */
  getFeatureById(id: Id): Feature {
    return this._dataManager.getEntityById(id, 'features') as Feature;
  }

  /**
   * Get specific entities by array of keys
   * @param {Array<string>} keys
   * @return {Array<Feature>}
   */
  getFeatures(keys: Array<string>): Array<Feature> {
    return this._dataManager.getItemsByKeys(keys, 'features') as Array<Feature>;
  }

  /**
   * Get a specific variable type defined in a specific feature
   * @param {string} key A feature's key
   * @param {string} variableName
   * @return {string|null}
   */
  getFeatureVariableType(key: string, variableName: string): string {
    const feature = this.getFeature(key);
    if (Object.prototype.hasOwnProperty.call(feature, 'variables')) {
      const variable = feature.variables.find((variable) => {
        return variable.key === variableName;
      });
      return variable?.type || null;
    }
    return null;
  }

  /**
   * Get a specific variable type defined in a specific feature by id
   * @param {Id} id A feature's id
   * @param {string} variableName
   * @return {string|null}
   */
  getFeatureVariableTypeById(id: Id, variableName: string): string {
    const feature = this.getFeatureById(id);
    if (Object.prototype.hasOwnProperty.call(feature, 'variables')) {
      const variable = feature.variables.find((variable) => {
        return variable.key === variableName;
      });
      return variable?.type || null;
    }
    return null;
  }

  /**
   * Check that feature is declared
   * @param {string} key Feature key
   * @return {boolean}
   */
  isFeatureDeclared(key: string): boolean {
    const declaredFeature = this._dataManager.getEntity(
      key,
      'features'
    ) as Feature;
    return !!declaredFeature;
  }

  /**
   * Get feature and its status
   * @param {Id} visitorId
   * @param {string} featureKey
   * @param {Record<string, any> | null} visitorProperties
   * @param {Record<string, any> | null} locationProperties
   * @param {boolean=} typeCasting Defaults to `true`
   * @param {Array<string>=} experienceKeys
   * @param {string=} environment
   * @return {BucketedFeature | RuleError | Array<BucketedFeature | RuleError>}
   */
  runFeature(
    visitorId: Id,
    featureKey: string,
    visitorProperties: Record<string, any> | null,
    locationProperties: Record<string, any> | null,
    typeCasting = true,
    experienceKeys?: Array<string>,
    environment?: string
  ): BucketedFeature | RuleError | Array<BucketedFeature | RuleError> {
    const declaredFeature = this._dataManager.getEntity(
      featureKey,
      'features'
    ) as Feature;

    if (declaredFeature) {
      const features = this.runFeatures(
        visitorId,
        visitorProperties,
        locationProperties,
        typeCasting,
        {
          features: [featureKey],
          experiences: experienceKeys
        },
        environment
      );
      if (arrayNotEmpty(features)) {
        if (features.length === 1) {
          // Return the bucketed feature
          return features[0];
        } else {
          // Return an array of bucketed features. It means the feature is used in different experiences and visitor has been bucketed to those variations
          return features;
        }
      }
      // Return disabled feature. Visitor was not bucketed
      return {
        id: declaredFeature.id,
        name: declaredFeature.name,
        key: featureKey,
        status: FeatureStatus.DISABLED
      } as BucketedFeature;
    } else {
      // The feature is not declared at all
      return {
        key: featureKey,
        status: FeatureStatus.DISABLED
      } as BucketedFeature;
    }
  }

  /**
   * Check is feature enabled.
   * @param {Id} visitorId
   * @param {string} featureKey
   * @param {Record<string, any> | null} visitorProperties
   * @param {Record<string, any> | null} locationProperties
   * @param {Array<string>=} experienceKeys
   * @param {string=} environment
   * @return {boolean}
   */
  isFeatureEnabled(
    visitorId: Id,
    featureKey: string,
    visitorProperties: Record<string, any> | null,
    locationProperties: Record<string, any> | null,
    experienceKeys?: Array<string>,
    environment?: string
  ): boolean {
    const declaredFeature = this._dataManager.getEntity(
      featureKey,
      'features'
    ) as Feature;
    if (declaredFeature) {
      const features = this.runFeatures(
        visitorId,
        visitorProperties,
        locationProperties,
        false,
        {
          features: [featureKey],
          experiences: experienceKeys
        },
        environment
      );
      return arrayNotEmpty(features);
    }
    return false;
  }

  /**
   * Get feature and its status
   * @param {Id} visitorId
   * @param {Id} featureId
   * @param {Record<string, any> | null} visitorProperties
   * @param {Record<string, any> | null} locationProperties
   * @param {boolean=} typeCasting Defaults to `true`
   * @param {Array<Id>=} experienceIds
   * @param {string=} environment
   * @return {BucketedFeature | Array<BucketedFeature> }
   */
  runFeatureById(
    visitorId: Id,
    featureId: Id,
    visitorProperties: Record<string, any> | null,
    locationProperties: Record<string, any> | null,
    typeCasting = true,
    experienceIds?: Array<Id>,
    environment?: string
  ): BucketedFeature | RuleError | Array<BucketedFeature | RuleError> {
    const declaredFeature = this._dataManager.getEntityById(
      featureId,
      'features'
    ) as Feature;

    if (declaredFeature) {
      const features = this.runFeatures(
        visitorId,
        visitorProperties,
        locationProperties,
        typeCasting,
        {
          features: [declaredFeature.key],
          experiences: this._dataManager
            .getEntitiesByIds(experienceIds, 'experiences')
            .map((e) => e.key)
        },
        environment
      );
      if (arrayNotEmpty(features)) {
        if (features.length === 1) {
          // Return the bucketed feature
          return features[0];
        } else {
          // Return rule errors if present
          const matchedErrors = features.filter((match) =>
            Object.values(RuleError).includes(match as RuleError)
          );
          if (matchedErrors.length) return matchedErrors as Array<RuleError>;
          // Return an array of bucketed features. It means the feature is used in different experiences and visitor has been bucketed to those variations
          return features;
        }
      }
      // Return disabled feature. Visitor was not bucketed
      return {
        id: featureId,
        name: declaredFeature.name,
        key: declaredFeature.key,
        status: FeatureStatus.DISABLED
      } as BucketedFeature;
    } else {
      // The feature is not declared at all
      return {
        id: featureId,
        status: FeatureStatus.DISABLED
      } as BucketedFeature;
    }
  }

  /**
   * Get features and their statuses
   * @param {Id} visitorId
   * @param {Record<string, any> | null} visitorProperties
   * @param {Record<string, any> | null} locationProperties
   * @param {boolean=} typeCasting Defaults to `true`
   * @param {Record<string, Array<string>>=} filter Filter records by experiences and/or features keys
   * @param {Array<string>} filter.experiences Array of experiences keys
   * @param {Array<string>} filter.features Array of features keys
   * @param {string=} environment
   * @return {Array<BucketedFeature | RuleError>}
   */
  runFeatures(
    visitorId: Id,
    visitorProperties: Record<string, any> | null,
    locationProperties: Record<string, any> | null,
    typeCasting = true,
    filter?: Record<string, Array<string>>,
    environment?: string
  ): Array<BucketedFeature | RuleError> {
    // Get list of declared features grouped by id
    const declaredFeatures = this.getListAsObject('id');

    const bucketedFeatures: Array<BucketedFeature> = [];

    // Retrieve all or filtered experiences
    const experiences = (
      filter && arrayNotEmpty(filter?.experiences)
        ? this._dataManager.getEntities(filter.experiences, 'experiences')
        : this._dataManager.getEntitiesList('experiences')
    ) as Array<Experience>;

    // Retrieve bucketed variations across the experiences
    const bucketedVariations = experiences
      .map((experience) => {
        const variation = this._dataManager.getBucketing(
          visitorId,
          experience?.key,
          visitorProperties,
          locationProperties,
          environment
        );
        if (Object.values(RuleError).includes(variation as RuleError))
          return variation as RuleError;
        return variation as BucketedVariation;
      })
      .filter(Boolean);

    // Return rule errors if present
    const matchedErrors = bucketedVariations.filter((match) =>
      Object.values(RuleError).includes(match as RuleError)
    );
    if (matchedErrors.length) return matchedErrors as Array<RuleError>;

    // Collect features from bucketed variations
    for (const k in bucketedVariations) {
      const bucketedVariation = bucketedVariations[k] as BucketedVariation;
      for (const v in bucketedVariation?.changes || []) {
        const changes = bucketedVariation?.changes?.[v]
          ?.data as FullStackFeatureChange;
        if (
          bucketedVariation?.changes?.[v]?.type !==
          VariationChangeType.FULLSTACK_FEATURE
        ) {
          this._loggerManager?.warn?.(MESSAGES.VARIATION_CHANGE_NOT_SUPPORTED);
          continue;
        }
        const featureId = changes?.feature_id;
        // Take the features filter into account
        if (!featureId) {
          this._loggerManager?.warn?.(MESSAGES.FEATURE_NOT_FOUND);
          continue;
        }
        if (
          (filter &&
            arrayNotEmpty(filter?.features) &&
            filter?.features?.indexOf(declaredFeatures[featureId]?.key) !==
              -1) ||
          !filter?.features
        ) {
          const variables = changes?.variables_data;

          if (!variables) {
            this._loggerManager?.warn?.(MESSAGES.FEATURE_VARIABLES_NOT_FOUND);
          }

          if (typeCasting && variables.constructor === Object) {
            // Convert variables values types
            for (const variableName in variables) {
              const variableDefinition = declaredFeatures[
                featureId
              ]?.variables?.find((obj) => {
                return obj.key === variableName;
              });
              if (variableDefinition?.type) {
                variables[variableName] = this.castType(
                  variables[variableName],
                  variableDefinition.type
                );
              } else {
                this._loggerManager?.warn?.(
                  MESSAGES.FEATURE_VARIABLES_TYPE_NOT_FOUND
                );
              }
            }
          }

          // Build the bucketed feature object
          const bucketedFeature = {
            ...{
              experienceId: bucketedVariation.experienceId,
              experienceName: bucketedVariation.experienceName,
              experienceKey: bucketedVariation.experienceKey
            },
            ...{
              key: declaredFeatures[featureId]?.key,
              name: declaredFeatures[featureId]?.name,
              id: featureId,
              status: FeatureStatus.ENABLED,
              variables: variables
            }
          };
          bucketedFeatures.push(bucketedFeature);
        }
      }
    }
    // Extend the list with not enabled features only if there is no features filter provided
    if (!filter?.features) {
      const bucketedFeaturesIds = bucketedFeatures.map((f) => f.id);
      // console.log(bucketedFeaturesIds)
      for (const k in declaredFeatures) {
        if (bucketedFeaturesIds.indexOf(declaredFeatures[k].id) === -1) {
          bucketedFeatures.push({
            id: declaredFeatures[k].id,
            name: declaredFeatures[k].name,
            key: declaredFeatures[k].key,
            status: FeatureStatus.DISABLED
          } as BucketedFeature);
        }
      }
    }
    return bucketedFeatures;
  }

  /**
   * Convert value's type
   * @param value
   * @param type
   */
  castType(value: any, type: VariableType): any {
    return castType(value, type);
  }
}
