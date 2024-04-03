/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {DataManagerInterface} from '@convertcom/js-sdk-data';
import {ExperienceManagerInterface} from './interfaces/experience-manager';
import {LogManagerInterface} from '@convertcom/js-sdk-logger';

import {
  Config,
  ConfigExperience,
  ExperienceVariationConfig,
  BucketedVariation,
  BucketingAttributes
} from '@convertcom/js-sdk-types';
import {MESSAGES, RuleError} from '@convertcom/js-sdk-enums';

/**
 * Provides experiences specific logic
 * @category Modules
 * @constructor
 * @implements {ExperienceManagerInterface}
 */
export class ExperienceManager implements ExperienceManagerInterface {
  private _dataManager: DataManagerInterface;
  private _loggerManager: LogManagerInterface | null;

  /**
   * @param config
   * @param {Record<string, any>} dependencies
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
      'ExperienceManager()',
      MESSAGES.EXPERIENCE_CONSTRUCTOR
    );
  }

  /**
   * Get a list of all entities
   * @return {Array<ConfigExperience>} Experiences list
   */
  getList(): Array<ConfigExperience> {
    return this._dataManager.getEntitiesList(
      'experiences'
    ) as Array<ConfigExperience>;
  }

  /**
   * Get the entity by key
   * @param {string} key
   * @return {ConfigExperience} An experience
   */
  getExperience(key: string): ConfigExperience {
    return this._dataManager.getEntity(key, 'experiences') as ConfigExperience;
  }

  /**
   * Get the entity by id
   * @param {string} id
   * @return {ConfigExperience} Get single experience
   */
  getExperienceById(id: string): ConfigExperience {
    return this._dataManager.getEntityById(
      id,
      'experiences'
    ) as ConfigExperience;
  }

  /**
   * Get specific entities by array of keys
   * @param {Array<string>} keys
   * @return {Array<ConfigExperience>}
   */
  getExperiences(keys: Array<string>): Array<ConfigExperience> {
    return this._dataManager.getItemsByKeys(
      keys,
      'experiences'
    ) as Array<ConfigExperience>;
  }

  /**
   * Select variation for specific visitor
   * @param {string} visitorId
   * @param {string} experienceKey
   * @param {BucketingAttributes} attributes
   * @param {Record<any, any>} attributes.locationProperties
   * @param {Record<any, any>} attributes.visitorProperties
   * @param {boolean=} attributes.updateVisitorProperties
   * @param {string=} attributes.forceVariationId
   * @param {boolean=} attributes.enableTracking
   * @param {string=} attributes.environment
   * @return {BucketedVariation | RuleError}
   */
  selectVariation(
    visitorId: string,
    experienceKey: string,
    attributes: BucketingAttributes
  ): BucketedVariation | RuleError {
    return this._dataManager.getBucketing(visitorId, experienceKey, attributes);
  }

  /**
   * Select variation for specific visitor
   * @param {string} visitorId
   * @param {string} experienceId
   * @param {BucketingAttributes} attributes
   * @param {Record<any, any>} attributes.locationProperties
   * @param {Record<any, any>} attributes.visitorProperties
   * @param {boolean=} attributes.updateVisitorProperties
   * @param {string=} attributes.forceVariationId
   * @param {boolean=} attributes.enableTracking
   * @param {string=} attributes.environment
   * @return {BucketedVariation | RuleError}
   */
  selectVariationById(
    visitorId: string,
    experienceId: string,
    attributes: BucketingAttributes
  ): BucketedVariation | RuleError {
    return this._dataManager.getBucketingById(
      visitorId,
      experienceId,
      attributes
    );
  }

  /**
   * Select all variations across all experiences for specific visitor
   * @param {string} visitorId
   * @param {BucketingAttributes} attributes
   * @param {Record<any, any>} attributes.locationProperties
   * @param {Record<any, any>} attributes.visitorProperties
   * @param {boolean=} attributes.updateVisitorProperties
   * @param {string=} attributes.forceVariationId
   * @param {boolean=} attributes.enableTracking
   * @param {string=} attributes.environment
   * @return {Array<BucketedVariation | RuleError>}
   */
  selectVariations(
    visitorId: string,
    attributes: BucketingAttributes
  ): Array<BucketedVariation | RuleError> {
    return this.getList()
      .map((experience) => {
        return this.selectVariation(visitorId, experience?.key, attributes);
      })
      .filter(Boolean);
  }

  /**
   * Get experience's variation by key
   * @param {string} experienceKey
   * @param {string}variationKey
   */
  getVariation(
    experienceKey: string,
    variationKey: string
  ): ExperienceVariationConfig {
    return this._dataManager.getSubItem(
      'experiences',
      experienceKey,
      'variations',
      variationKey,
      'key',
      'key'
    ) as ExperienceVariationConfig;
  }

  /**
   * Get experience's variation by id
   * @param experienceId
   * @param variationId
   */
  getVariationById(
    experienceId: string,
    variationId: string
  ): ExperienceVariationConfig {
    return this._dataManager.getSubItem(
      'experiences',
      experienceId,
      'variations',
      variationId,
      'id',
      'id'
    ) as ExperienceVariationConfig;
  }
}
