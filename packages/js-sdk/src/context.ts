/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {ContextInterface} from './interfaces/context';
import {EventManagerInterface} from './interfaces/event-manager';
import {ExperienceManagerInterface} from './interfaces/experience-manager';
import {FeatureManagerInterface} from './interfaces/feature-manager';
import {LogManagerInterface} from './interfaces/log-manager';
import {DataManagerInterface} from './interfaces/data-manager';

import {Config} from './types/Config';
import {Id} from './types/Id';

import {ERROR_MESSAGES} from './enums/dictionary';
import {SystemEvents} from './enums/system-events';
import {BucketedFeature} from './types/BucketedFeature';
import {BucketedVariation} from './types/BucketedVariation';
import {BucketingAttributes} from './types/BucketingAttributes';
import {ConversionAttributes} from './types/ConversionAttributes';
import {objectDeepMerge} from './utils/object-utils';
import {SegmentsKeys} from './enums/segments/segments-keys';
import {SegmentsData} from './types/SegmentsData';
import {SegmentsAttributes} from './types/SegmentsAttributes';
import {SegmentsManagerInterface} from './interfaces/segments-manager';

/**
 * Provides visitor context
 * @category Main
 * @constructor
 * @implements {ContextInterface}
 */
export class Context implements ContextInterface {
  private _eventManager: EventManagerInterface;
  private _experienceManager: ExperienceManagerInterface;
  private _featureManager: FeatureManagerInterface;
  private _dataManager: DataManagerInterface;
  private _segmentsManager: SegmentsManagerInterface;
  private _loggerManager: LogManagerInterface;
  private _config: Config;
  private _visitorId: Id;
  private _visitorAttributes: Record<string, any>;
  private _environment: string;

  /**
   * @param {Config} config
   * @param {Object} dependencies
   * @param {ApiManagerInterface} dependencies.apiManager
   * @param {EventManagerInterface} dependencies.eventManager
   * @param {ExperienceManagerInterface} dependencies.experienceManager
   * @param {FeatureManagerInterface} dependencies.featureManager
   * @param {DataManagerInterface} dependencies.dataManager
   * @param {LogManagerInterface} dependencies.loggerManager
   */
  constructor(
    config: Config,
    visitorId: Id,
    {
      eventManager,
      experienceManager,
      featureManager,
      segmentsManager,
      dataManager,
      loggerManager
    }: {
      eventManager: EventManagerInterface;
      experienceManager: ExperienceManagerInterface;
      featureManager: FeatureManagerInterface;
      segmentsManager: SegmentsManagerInterface;
      dataManager: DataManagerInterface;
      loggerManager?: LogManagerInterface;
    },
    visitorAttributes?: Record<string, any>
  ) {
    this._environment = config?.environment;
    this._visitorId = visitorId;

    this._config = config;
    this._eventManager = eventManager;
    this._experienceManager = experienceManager;
    this._featureManager = featureManager;
    this._dataManager = dataManager;
    this._segmentsManager = segmentsManager;
    this._loggerManager = loggerManager;

    if (visitorAttributes && visitorAttributes.constructor === Object) {
      const {attributes, segments} =
        this.processVisitorAttributes(visitorAttributes);
      if (attributes) this._visitorAttributes = attributes;
      if (segments) segmentsManager.putSegments(visitorId, segments);
    }
  }

  /**
   * Get variation from specific experience
   * @param {string} experienceKey An experience's key that should be activated
   * @param {BucketingAttributes=} attributes An object that specifies attributes for the visitor
   * @param {Record<any, any>=} attributes.locationProperties An object of key-value pairs that are used for location matching
   * @param {Record<any, any>=} attributes.visitorProperties An object of key-value pairs that are used for audience targeting
   * @param {string=} attributes.environment Overwrite the environment
   * @return {BucketedVariation | null}
   */
  runExperience(
    experienceKey: string,
    attributes?: BucketingAttributes
  ): BucketedVariation | null {
    if (!this._visitorId) {
      this._loggerManager?.error?.(ERROR_MESSAGES.VISITOR_ID_REQUIRED);
      return;
    }
    const visitorAttributes = this.getVisitorAttributes(
      attributes?.visitorProperties
    );
    const bucketedVariation = this._experienceManager.selectVariation(
      this._visitorId,
      experienceKey,
      visitorAttributes, // represents audiences
      attributes?.locationProperties, // represents site_area/locations
      attributes?.environment || this._environment
    );
    if (bucketedVariation) {
      this._eventManager.fire(
        SystemEvents.BUCKETING,
        {
          visitorId: this._visitorId,
          experienceKey,
          variationKey: bucketedVariation.key
        },
        null,
        true
      );
    }
    return bucketedVariation;
  }

  /**
   * Get variations across all experiences
   * @param {BucketingAttributes=} attributes An object that specifies attributes for the visitor
   * @param {string=} attributes.locationProperties An object of key-value pairs that are used for location matching
   * @param {Record<any, any>=} attributes.visitorProperties An object of key-value pairs that are used for audience targeting
   * @param {string=} attributes.environment Overwrite the environment
   * @return {Array<BucketedVariation>}
   */
  runExperiences(attributes?: BucketingAttributes): Array<BucketedVariation> {
    if (!this._visitorId) {
      this._loggerManager?.error?.(ERROR_MESSAGES.VISITOR_ID_REQUIRED);
      return;
    }
    const visitorAttributes = this.getVisitorAttributes(
      attributes?.visitorProperties
    );
    const bucketedVariations = this._experienceManager.selectVariations(
      this._visitorId,
      visitorAttributes,
      attributes?.locationProperties,
      attributes?.environment || this._environment
    );
    bucketedVariations.forEach(({experienceKey, key}) => {
      this._eventManager.fire(
        SystemEvents.BUCKETING,
        {
          visitorId: this._visitorId,
          experienceKey,
          variationKey: key
        },
        null,
        true
      );
    });
    return bucketedVariations;
  }

  /**
   * Get feature and its status
   * @param {string} key A feature key
   * @param {BucketingAttributes=} attributes An object that specifies attributes for the visitor
   * @param {string=} attributes.locationProperties An object of key-value pairs that are used for location matching
   * @param {Record<any, any>=} attributes.visitorProperties An object of key-value pairs that are used for audience targeting
   * @param {string=} attributes.environment Overwrite the environment
   * @param {boolean=} attributes.typeCasting Control automatic type conversion to the variable's defined type. Does not do any JSON validation. Defaults to `true`
   * @param {Array<string>=} attributes.experienceKeys Use only specific experiences
   * @return {BucketedFeature | Array<BucketedFeature>}
   */
  runFeature(
    key: string,
    attributes?: BucketingAttributes
  ): BucketedFeature | Array<BucketedFeature> {
    if (!this._visitorId) {
      this._loggerManager?.error?.(ERROR_MESSAGES.VISITOR_ID_REQUIRED);
      return;
    }
    const visitorAttributes = this.getVisitorAttributes(
      attributes?.visitorProperties
    );
    const bucketedFeature = this._featureManager.runFeature(
      this._visitorId,
      key,
      visitorAttributes,
      attributes?.locationProperties,
      Object.prototype.hasOwnProperty.call(attributes || {}, 'typeCasting')
        ? attributes.typeCasting
        : true,
      attributes?.experienceKeys,
      attributes?.environment || this._environment
    );
    if (Array.isArray(bucketedFeature)) {
      bucketedFeature.forEach(({experienceKey, status}) => {
        this._eventManager.fire(
          SystemEvents.BUCKETING,
          {
            visitorId: this._visitorId,
            experienceKey,
            featureKey: key,
            status
          },
          null,
          true
        );
      });
    } else if (bucketedFeature) {
      this._eventManager.fire(
        SystemEvents.BUCKETING,
        {
          visitorId: this._visitorId,
          experienceKey: bucketedFeature.experienceKey,
          featureKey: key,
          status: bucketedFeature.status
        },
        null,
        true
      );
    }
    return bucketedFeature;
  }

  /**
   * Get features and their statuses
   * @param {BucketingAttributes=} attributes An object that specifies attributes for the visitor
   * @param {string=} attributes.locationProperties An object of key-value pairs that are used for location matching
   * @param {Record<any, any>=} attributes.visitorProperties An object of key-value pairs that are used for audience targeting
   * @param {string=} attributes.environment Overwrite the environment
   * @param {boolean=} attributes.typeCasting Control automatic type conversion to the variable's defined type. Does not do any JSON validation. Defaults to `true`
   * @return {Array<BucketedFeature>}
   */
  runFeatures(attributes?: BucketingAttributes): Array<BucketedFeature> {
    if (!this._visitorId) {
      this._loggerManager?.error?.(ERROR_MESSAGES.VISITOR_ID_REQUIRED);
      return;
    }
    const visitorAttributes = this.getVisitorAttributes(
      attributes?.visitorProperties
    );
    const bucketedFeatures = this._featureManager.runFeatures(
      this._visitorId,
      visitorAttributes,
      attributes?.locationProperties,
      Object.prototype.hasOwnProperty.call(attributes || {}, 'typeCasting')
        ? attributes.typeCasting
        : true,
      null,
      attributes?.environment || this._environment
    );
    bucketedFeatures.forEach(({experienceKey, key, status}) => {
      this._eventManager.fire(
        SystemEvents.BUCKETING,
        {
          visitorId: this._visitorId,
          experienceKey,
          featureKey: key,
          status
        },
        null,
        true
      );
    });
    return bucketedFeatures;
  }

  /**
   * Trigger Conversion
   * @param {Id} goalKey A goal key
   * @param {ConversionAttributes=} attributes An object that specifies attributes for the visitor
   * @param {Record<string, any>=} attributes.ruleData An object of key-value pairs that are used for goal matching
   * @param {Array<Record<GoalDataKey, number>>=} attributes.conversionData An object of key-value pairs that are used for audience targeting
   */
  trackConversion(goalKey: Id, attributes?: ConversionAttributes): void {
    if (!this._visitorId) {
      this._loggerManager?.error?.(ERROR_MESSAGES.VISITOR_ID_REQUIRED);
      return;
    }

    const goalRule = attributes?.ruleData;
    const goalData = attributes?.conversionData;
    if (goalData) {
      if (!Array.isArray(goalData)) {
        this._loggerManager?.error?.(ERROR_MESSAGES.GOAL_DATA_NOT_VALID);
        return;
      }
    }

    const segments = this._segmentsManager.getSegments(this._visitorId);
    this._dataManager.convert(
      this._visitorId,
      goalKey,
      goalRule,
      goalData,
      segments
    );

    this._eventManager.fire(
      SystemEvents.CONVERSION,
      {
        visitorId: this._visitorId,
        goalKey
      },
      null,
      true
    );
  }

  /**
   * Set default segments
   * @param {SegmentsData} segments A segment key
   */
  setDefaultSegments(segments: SegmentsData): void {
    this._segmentsManager.putSegments(this._visitorId, segments);
  }

  /**
   * Set Custom segments
   * @param {Array<string>} segmentKeys A list of segment keys
   * @param {SegmentsAttributes=} attributes An object that specifies attributes for the visitor
   * @param {Record<string, any>=} attributes.ruleData An object of key-value pairs that are used for segments matching
   */
  setCustomSegments(
    segmentKeys: Array<string>,
    attributes?: SegmentsAttributes
  ): void {
    if (!this._visitorId) {
      this._loggerManager?.error?.(ERROR_MESSAGES.VISITOR_ID_REQUIRED);
      return;
    }
    const segmentsRule = this.getVisitorAttributes(attributes?.ruleData);
    this._segmentsManager.selectCustomSegments(
      this._visitorId,
      segmentKeys,
      segmentsRule
    );
  }

  /**
   * Get visitor properties
   * @param {Record<string, any>=} visitorAttributes An object of key-value pairs that are used for audience targeting
   * @return {Record<string, any>}
   */
  private getVisitorAttributes(
    visitorAttributes?: Record<string, any>
  ): Record<string, any> {
    return visitorAttributes
      ? objectDeepMerge(this._visitorAttributes || {}, visitorAttributes)
      : this._visitorAttributes;
  }

  /**
   * Extract segments from other attribues in Visitor properties
   * @param {Record<string, any>=} visitorAttributes An object of key-value pairs that are used for audience targeting
   * @return {Record<string, any>}
   */
  private processVisitorAttributes(
    visitorAttributes: Record<string, any>
  ): Record<string, any> {
    const segmentsKeys = Object.values(SegmentsKeys).map(
      (key) => key as string
    );
    const segments = {};
    const attributes = {};
    for (const key in visitorAttributes) {
      if (segmentsKeys.includes(key)) {
        segments[key] = visitorAttributes[key];
      } else {
        attributes[key] = visitorAttributes[key];
      }
    }
    return {
      attributes: Object.keys(attributes).length ? attributes : null,
      segments: Object.keys(segments).length ? segments : null
    };
  }
}
