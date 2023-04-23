/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {DataManagerInterface} from './interfaces/data-manager';
import {ExperienceManagerInterface} from './interfaces/experience-manager';
import {LogManagerInterface} from '@convertcom/logger';

import {
  Config,
  Experience,
  Id,
  Variation,
  BucketedVariation
} from '@convertcom/types';
import {MESSAGES} from '@convertcom/enums';

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
    this._loggerManager?.trace?.(MESSAGES.EXPERIENCE_CONSTRUCTOR);
  }

  /**
   * Get a list of all entities
   * @return {Array<Experience>} Experiences list
   */
  getList(): Array<Experience> {
    return this._dataManager.getEntitiesList(
      'experiences'
    ) as Array<Experience>;
  }

  /**
   * Get the entity by key
   * @param {string} key
   * @return {Experience} An experience
   */
  getExperience(key: string): Experience {
    return this._dataManager.getEntity(key, 'experiences') as Experience;
  }

  /**
   * Get the entity by id
   * @param {Id} id
   * @return {Experience} Get single experience
   */
  getExperienceById(id: Id): Experience {
    return this._dataManager.getEntityById(id, 'experiences') as Experience;
  }

  /**
   * Get specific entities by array of keys
   * @param {Array<string>} keys
   * @return {Array<Experience>}
   */
  getExperiences(keys: Array<string>): Array<Experience> {
    return this._dataManager.getItemsByKeys(
      keys,
      'experiences'
    ) as Array<Experience>;
  }

  /**
   * Select variation for specific visitor
   * @param {Id} visitorId
   * @param {string} experienceKey
   * @param {Record<string, any>} visitorProperties
   * @param {string} locationProperties
   * @param {string=} environment
   * @return {BucketedVariation | null}
   */
  selectVariation(
    visitorId: Id,
    experienceKey: string,
    visitorProperties: Record<string, any>,
    locationProperties: Record<string, any>,
    environment?: string
  ): BucketedVariation | null {
    return this._dataManager.getBucketing(
      visitorId,
      experienceKey,
      visitorProperties,
      locationProperties,
      environment
    );
  }

  /**
   * Select variation for specific visitor
   * @param {Id} visitorId
   * @param {Id} experienceId
   * @param {Record<string, any>} visitorProperties
   * @param {string} locationProperties
   * @param {string=} environment
   * @return {BucketedVariation | null}
   */
  selectVariationById(
    visitorId: Id,
    experienceId: Id,
    visitorProperties: Record<string, any>,
    locationProperties: Record<string, any>,
    environment?: string
  ): BucketedVariation | null {
    return this._dataManager.getBucketingById(
      visitorId,
      experienceId,
      visitorProperties,
      locationProperties,
      environment
    );
  }

  /**
   * Select all variations across all experiences for specific visitor
   * @param {Id} visitorId
   * @param {Record<string, any>} visitorProperties
   * @param {string} locationProperties
   * @param {string=} environment
   * @return {Array<BucketedVariation>}
   */
  selectVariations(
    visitorId: Id,
    visitorProperties: Record<string, any>,
    locationProperties: Record<string, any>,
    environment?: string
  ): Array<BucketedVariation> {
    return this.getList()
      .map((experience) => {
        return this.selectVariation(
          visitorId,
          experience?.key,
          visitorProperties,
          locationProperties,
          environment
        );
      })
      .filter(Boolean);
  }

  /**
   * Get experience's variation by key
   * @param {string} experienceKey
   * @param {string}variationKey
   */
  getVariation(experienceKey: string, variationKey: string): Variation {
    return this._dataManager.getSubItem(
      'experiences',
      experienceKey,
      'variations',
      variationKey,
      'key',
      'key'
    ) as Variation;
  }

  /**
   * Get experience's variation by id
   * @param experienceId
   * @param variationId
   */
  getVariationById(experienceId: Id, variationId: Id): Variation {
    return this._dataManager.getSubItem(
      'experiences',
      experienceId,
      'variations',
      variationId,
      'id',
      'id'
    ) as Variation;
  }
}
