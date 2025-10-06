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
  ConfigFeature,
  BucketedFeature,
  IdentityField,
  VariableType,
  ConfigExperience,
  BucketingAttributes,
  ExperienceVariationConfig
} from '@convertcom/js-sdk-types';
import {
  MESSAGES,
  FeatureStatus,
  RuleError,
  VariationChangeType
} from '@convertcom/js-sdk-enums';

import {
  castType,
  arrayNotEmpty,
  objectNotEmpty
} from '@convertcom/js-sdk-utils';
import {BucketedVariation} from '@convertcom/js-sdk-types';

type RustFeatureDecision = {
  id: string;
  key: string;
  name?: string;
  status: 'Enabled' | 'Disabled';
  experience_id?: string;
  experience_key?: string;
  variation_id?: string;
  variation_key?: string;
  variables?: Record<string, any> | null;
};

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
    this._loggerManager?.trace?.(
      'FeatureManager()',
      MESSAGES.FEATURE_CONSTRUCTOR
    );
  }

  /**
   * Get a list of all entities
   * @return {Array<ConfigFeature>} Features list
   */
  getList(): Array<ConfigFeature> {
    return this._dataManager.getEntitiesList(
      'features'
    ) as Array<ConfigFeature>;
  }

  /**
   * Get a list of all entities as object of entities grouped by identity field
   * @param {IdentityField=} field A field to group entities defaults to `id`
   * @return {Record<string, ConfigFeature>} Features list
   */
  getListAsObject(field: IdentityField = 'id'): Record<string, ConfigFeature> {
    return this._dataManager.getEntitiesListObject('features', field) as Record<
      string,
      ConfigFeature
    >;
  }

  /**
   * Get the entity by key
   * @param {string} key
   * @return {ConfigFeature}
   */
  getFeature(key: string): ConfigFeature {
    return this._dataManager.getEntity(key, 'features') as ConfigFeature;
  }

  /**
   * Get the entity by id
   * @param {string} id
   * @return {ConfigFeature}
   */
  getFeatureById(id: string): ConfigFeature {
    return this._dataManager.getEntityById(id, 'features') as ConfigFeature;
  }

  /**
   * Get specific entities by array of keys
   * @param {Array<string>} keys
   * @return {Array<ConfigFeature>}
   */
  getFeatures(keys: Array<string>): Array<ConfigFeature> {
    return this._dataManager.getItemsByKeys(
      keys,
      'features'
    ) as Array<ConfigFeature>;
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
   * @param {string} id A feature's id
   * @param {string} variableName
   * @return {string|null}
   */
  getFeatureVariableTypeById(id: string, variableName: string): string {
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
   * @param {string} key ConfigFeature key
   * @return {boolean}
   */
  isFeatureDeclared(key: string): boolean {
    const declaredFeature = this._dataManager.getEntity(
      key,
      'features'
    ) as ConfigFeature;
    return !!declaredFeature;
  }

  /**
   * Get feature and its status
   * @param {string} visitorId
   * @param {string} featureKey
   * @param {BucketingAttributes} attributes
   * @param {Record<any, any>} attributes.locationProperties
   * @param {Record<any, any>} attributes.visitorProperties
   * @param {boolean=} attributes.updateVisitorProperties
   * @param {boolean=} attributes.typeCasting Defaults to `true`
   * @param {string=} attributes.environment
   * @param {Array<string>=} experienceKeys
   * @return {BucketedFeature | RuleError | Array<BucketedFeature | RuleError>}
   */
  runFeature(
    visitorId: string,
    featureKey: string,
    attributes: BucketingAttributes,
    experienceKeys?: Array<string>
  ): BucketedFeature | RuleError | Array<BucketedFeature | RuleError> {
    const declaredFeature = this._dataManager.getEntity(
      featureKey,
      'features'
    ) as ConfigFeature;

    if (declaredFeature) {
      const features = this.runFeatures(visitorId, attributes, {
        features: [featureKey],
        experiences: experienceKeys
      });
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
   * @param {string} visitorId
   * @param {string} featureKey
   * @param {BucketingAttributes} attributes
   * @param {Record<any, any>} attributes.locationProperties
   * @param {Record<any, any>} attributes.visitorProperties
   * @param {string=} attributes.environment
   * @param {Array<string>=} experienceKeys
   * @return {boolean}
   */
  isFeatureEnabled(
    visitorId: string,
    featureKey: string,
    attributes: BucketingAttributes,
    experienceKeys?: Array<string>
  ): boolean {
    const declaredFeature = this._dataManager.getEntity(
      featureKey,
      'features'
    ) as ConfigFeature;
    if (declaredFeature) {
      const features = this.runFeatures(visitorId, attributes, {
        features: [featureKey],
        experiences: experienceKeys
      });
      return arrayNotEmpty(features);
    }
    return false;
  }

  /**
   * Get feature and its status
   * @param {string} visitorId
   * @param {string} featureId
   * @param {BucketingAttributes} attributes
   * @param {Record<any, any>} attributes.locationProperties
   * @param {Record<any, any>} attributes.visitorProperties
   * @param {boolean=} attributes.updateVisitorProperties
   * @param {boolean=} attributes.typeCasting Defaults to `true`
   * @param {string=} attributes.environment
   * @param {Array<string>=} experienceIds
   * @return {BucketedFeature | Array<BucketedFeature> }
   */
  runFeatureById(
    visitorId: string,
    featureId: string,
    attributes: BucketingAttributes,
    experienceIds?: Array<string>
  ): BucketedFeature | RuleError | Array<BucketedFeature | RuleError> {
    const declaredFeature = this._dataManager.getEntityById(
      featureId,
      'features'
    ) as ConfigFeature;

    if (declaredFeature) {
      const features = this.runFeatures(visitorId, attributes, {
        features: [declaredFeature.key],
        experiences: this._dataManager
          .getEntitiesByIds(experienceIds, 'experiences')
          .map((e) => e.key)
      });
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
   * @param {string} visitorId
   * @param {BucketingAttributes} attributes
   * @param {Record<any, any>} attributes.locationProperties
   * @param {Record<any, any>} attributes.visitorProperties
   * @param {boolean=} attributes.updateVisitorProperties
   * @param {boolean=} attributes.typeCasting Defaults to `true`
   * @param {string=} attributes.environment
   * @param {Record<string, Array<string>>=} filter Filter records by experiences and/or features keys
   * @param {Array<string>} filter.experiences Array of experiences keys
   * @param {Array<string>} filter.features Array of features keys
   * @return {Array<BucketedFeature | RuleError>}
   */
  runFeatures(
    visitorId: string,
    attributes: BucketingAttributes,
    filter?: Record<string, Array<string>>
  ): Array<BucketedFeature | RuleError> {
    const {typeCasting = true} = attributes;
    // Get list of declared features grouped by id
    const declaredFeatures = this.getListAsObject('id');

    const bucketedFeatures: Array<BucketedFeature> = [];

    // Retrieve all or filtered experiences
    const experiences = (
      filter && arrayNotEmpty(filter?.experiences)
        ? this._dataManager.getEntities(filter.experiences, 'experiences')
        : this._dataManager.getEntitiesList('experiences')
    ) as Array<ConfigExperience>;

    // Retrieve bucketed variations across the experiences
    const bucketedVariations = experiences
      .map((experience) => {
        const variation = this._dataManager.getBucketing(
          visitorId,
          experience?.key,
          attributes
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

    const rustAggregated = this._maybeAggregateFeaturesWithRust(
      bucketedVariations as Array<BucketedVariation>,
      filter,
      typeCasting
    );
    if (rustAggregated) return rustAggregated;

    // Collect features from bucketed variations
    for (const k in bucketedVariations) {
      const bucketedVariation = bucketedVariations[k] as BucketedVariation;
      for (const v in bucketedVariation?.changes || []) {
        const changes = bucketedVariation?.changes?.[v]?.data;
        if (
          bucketedVariation?.changes?.[v]?.type !==
          VariationChangeType.FULLSTACK_FEATURE
        ) {
          this._loggerManager?.warn?.(
            'FeatureManager.runFeatures()',
            MESSAGES.VARIATION_CHANGE_NOT_SUPPORTED
          );
          continue;
        }
        const featureId = changes?.feature_id;
        // Take the features filter into account
        if (!featureId) {
          this._loggerManager?.warn?.(
            'FeatureManager.runFeatures()',
            MESSAGES.FEATURE_NOT_FOUND
          );
          continue;
        }
        if (
          (filter &&
            arrayNotEmpty(filter?.features) &&
            filter?.features?.indexOf(
              declaredFeatures[String(featureId)]?.key
            ) !== -1) ||
          !filter?.features
        ) {
          const variables = changes?.variables_data;

          if (!variables) {
            this._loggerManager?.warn?.(
              'FeatureManager.runFeatures()',
              MESSAGES.FEATURE_VARIABLES_NOT_FOUND
            );
          }

          if (typeCasting && objectNotEmpty(variables)) {
            // Convert variables values types
            for (const variableName in variables as object) {
              const variableDefinition = declaredFeatures[
                String(featureId)
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
                  'FeatureManager.runFeatures()',
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
              key: declaredFeatures[String(featureId)]?.key,
              name: declaredFeatures[String(featureId)]?.name,
              id: String(featureId),
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

  private _maybeAggregateFeaturesWithRust(
    variations: Array<BucketedVariation>,
    filter: Record<string, Array<string>> | undefined,
    typeCasting: boolean
  ): Array<BucketedFeature> | null {
    if (!this._dataManager?.isRustDeciderEnabled()) return null;
    const variationSummaries = variations
      .map((variation) => this._toRustVariationSummary(variation))
      .filter(Boolean) as Array<{
        experience_id: string;
        experience_key: string;
        variation: ExperienceVariationConfig;
        allocation?: [number, number];
      }>;

    if (!variationSummaries.length) return null;

    const filters = filter
      ? {
          feature_keys: filter.features,
          experience_keys: filter.experiences
        }
      : undefined;

    const response = this._dataManager.aggregateFeaturesWithRust(
      variationSummaries,
      {
        filters,
        typeCasting
      }
    );

    if (!response) return null;

    return response.features.map((decision: RustFeatureDecision) =>
      this._mapRustFeatureDecision(decision as RustFeatureDecision)
    );
  }

  private _toRustVariationSummary(
    variation: BucketedVariation
  ): {
    experience_id: string;
    experience_key: string;
    variation: ExperienceVariationConfig;
    allocation?: [number, number];
  } | null {
    const {
      experienceId,
      experienceKey,
      experienceName: _experienceName,
      bucketingAllocation,
      ...variationPayload
    } = variation;

    if (!experienceId || !experienceKey || !variationPayload?.id) return null;

    return {
      experience_id: String(experienceId),
      experience_key: String(experienceKey),
      variation: variationPayload as ExperienceVariationConfig,
      allocation: undefined
    };
  }

  private _mapRustFeatureDecision(decision: RustFeatureDecision): BucketedFeature {
    const status =
      decision.status === 'Enabled'
        ? FeatureStatus.ENABLED
        : FeatureStatus.DISABLED;

    const feature: BucketedFeature = {
      id: decision.id,
      key: decision.key,
      name: decision.name,
      status,
      experienceId: decision.experience_id || undefined,
      experienceKey: decision.experience_key || undefined,
      variables: decision.variables || undefined
    } as BucketedFeature;

    if (decision.experience_id) {
      const experience = this._dataManager.getEntityById(
        decision.experience_id,
        'experiences'
      ) as ConfigExperience;
      if (experience?.name) {
        feature.experienceName = experience.name;
      }
    }

    return feature;
  }
}
