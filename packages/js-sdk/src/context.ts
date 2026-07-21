/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {ContextInterface, PreviewInput} from './interfaces/context';
import {EventManagerInterface} from '@convertcom/js-sdk-event';
import {ExperienceManagerInterface} from '@convertcom/js-sdk-experience';
import {FeatureManagerInterface} from './interfaces/feature-manager';
import {LogManagerInterface} from '@convertcom/js-sdk-logger';
import {DataManagerInterface} from '@convertcom/js-sdk-data';

import {
  Config,
  BucketedFeature,
  BucketedVariation,
  BucketingAttributes,
  ConversionAttributes,
  VisitorSegments,
  SegmentsAttributes,
  Entity,
  ConfigExperience,
  ExperienceVariationConfig,
  StoreData
} from '@convertcom/js-sdk-types';

import {
  BucketingError,
  ERROR_MESSAGES,
  EntityType,
  RuleError,
  SystemEvents
} from '@convertcom/js-sdk-enums';
import {objectDeepMerge, objectNotEmpty} from '@convertcom/js-sdk-utils';
import {SegmentsManagerInterface} from '@convertcom/js-sdk-segments';
import {ApiManagerInterface} from '@convertcom/js-sdk-api';

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
  private _apiManager: ApiManagerInterface;
  private _loggerManager: LogManagerInterface;
  private _config: Config;
  private _visitorId: string;
  private _visitorProperties: Record<string, any>;
  private _environment: string;
  /**
   * qs-02: preview state for this Context instance only. Never shared across
   * Context instances on the same SDK instance (isolation, AC6). `null` means
   * this context runs normally.
   */
  private _preview: {
    experienceId: string;
    variationId: string;
    experience: ConfigExperience;
    experienceKey: string;
  } | null = null;

  /**
   * @param {Config} config
   * @param {Object} dependencies
   * @param {ApiManagerInterface} dependencies.apiManager
   * @param {EventManagerInterface} dependencies.eventManager
   * @param {ExperienceManagerInterface} dependencies.experienceManager
   * @param {FeatureManagerInterface} dependencies.featureManager
   * @param {DataManagerInterface} dependencies.dataManager
   * @param {ApiManagerInterface} dependencies.apiManager
   * @param {LogManagerInterface} dependencies.loggerManager
   */
  constructor(
    config: Config,
    visitorId: string,
    {
      eventManager,
      experienceManager,
      featureManager,
      segmentsManager,
      dataManager,
      apiManager,
      loggerManager
    }: {
      eventManager: EventManagerInterface;
      experienceManager: ExperienceManagerInterface;
      featureManager: FeatureManagerInterface;
      segmentsManager: SegmentsManagerInterface;
      dataManager: DataManagerInterface;
      apiManager: ApiManagerInterface;
      loggerManager?: LogManagerInterface;
    },
    visitorProperties?: Record<string, any>
  ) {
    this._environment = config?.environment;
    this._visitorId = visitorId;

    this._config = config;
    this._eventManager = eventManager;
    this._experienceManager = experienceManager;
    this._featureManager = featureManager;
    this._dataManager = dataManager;
    this._segmentsManager = segmentsManager;
    this._apiManager = apiManager;
    this._loggerManager = loggerManager;

    if (objectNotEmpty(visitorProperties)) {
      const {properties} =
        this._dataManager.filterReportSegments(visitorProperties);
      if (properties) this._visitorProperties = properties;
      segmentsManager.putSegments(visitorId, visitorProperties);
    }
  }

  /**
   * qs-02: put this Context into preview mode for a single experience/variation
   * pair. Once set:
   *  - `runExperience(experienceKey)` for the matching `experienceKey` returns
   *    the forced preview decision directly (bypassing audiences/locations/
   *    bucketing) and fires no `SystemEvents.BUCKETING` event.
   *  - Every other `run*` call is suppressed from tracking/persisting
   *    (`enableTracking: false`, `enableStorage: false`) and fires no
   *    `SystemEvents.BUCKETING` event.
   *  - `trackConversion()` is a full no-op (zero-trace, AC5).
   * Preview state is local to THIS Context instance only (AC6 isolation) and
   * never mutates shared `DataManager` config.
   * Inert (leaves `_preview` unset) when `experienceId`/`variationId` are
   * missing, when the experience cannot be resolved (neither via shared
   * config nor via the `?exp=` low-cache fetch), or when the variation is not
   * present on the resolved experience (AC7).
   * @param {Object} input
   * @param {string} input.experienceId
   * @param {string} input.variationId
   * @return {Promise<void>}
   */
  async setPreview(input: PreviewInput): Promise<void> {
    const experienceId = input?.experienceId;
    const variationId = input?.variationId;
    if (!experienceId || !variationId) {
      this._loggerManager?.warn?.(
        'Context.setPreview()',
        ERROR_MESSAGES.PREVIEW_INPUT_REQUIRED
      );
      return;
    }

    let experience = this._dataManager.getEntityById(
      experienceId,
      EntityType.EXPERIENCE
    ) as ConfigExperience;

    if (!experience) {
      try {
        const config =
          await this._apiManager.getConfigByExperience(experienceId);
        experience = (config?.experiences || []).find(
          (candidate) => String(candidate?.id) === String(experienceId)
        ) as ConfigExperience;
      } catch (error) {
        this._loggerManager?.warn?.(
          'Context.setPreview()',
          ERROR_MESSAGES.PREVIEW_EXPERIENCE_NOT_FOUND,
          error
        );
        this._preview = null;
        return;
      }
    }

    if (!experience) {
      this._loggerManager?.warn?.(
        'Context.setPreview()',
        ERROR_MESSAGES.PREVIEW_EXPERIENCE_NOT_FOUND
      );
      this._preview = null;
      return;
    }

    const decision = this._dataManager.getPreviewDecision(
      experience,
      variationId
    );
    if (!decision) {
      this._loggerManager?.warn?.(
        'Context.setPreview()',
        ERROR_MESSAGES.PREVIEW_VARIATION_NOT_FOUND
      );
      this._preview = null;
      return;
    }

    this._preview = {
      experienceId,
      variationId,
      experience,
      experienceKey: experience.key
    };
  }

  /**
   * Get variation from specific experience
   * @param {string} experienceKey An experience's key that should be activated
   * @param {BucketingAttributes=} attributes An object that specifies attributes for the visitor
   * @param {Record<any, any>=} attributes.locationProperties An object of key-value pairs that are used for location matching
   * @param {Record<any, any>=} attributes.visitorProperties An object of key-value pairs that are used for audience targeting
   * @param {boolean=} attributes.updateVisitorProperties Decide whether to update visitor properties upon bucketing
   * @param {string=} attributes.environment Overwrite the environment
   * @return {BucketedVariation | RuleError | BucketingError}
   */
  runExperience(
    experienceKey: string,
    attributes?: BucketingAttributes
  ): BucketedVariation | RuleError | BucketingError {
    if (!this._visitorId) {
      this._loggerManager?.error?.(
        'Context.runExperience()',
        ERROR_MESSAGES.VISITOR_ID_REQUIRED
      );
      return;
    }
    // qs-02: force the preview decision for the previewed experience -- bypass
    // audiences/locations/bucketing entirely and never fire BUCKETING (AC3, AC5).
    if (this._preview && experienceKey === this._preview.experienceKey) {
      return this._dataManager.getPreviewDecision(
        this._preview.experience,
        this._preview.variationId
      );
    }
    const visitorProperties = this.getVisitorProperties(
      attributes?.visitorProperties
    );
    const bucketedVariation = this._experienceManager.selectVariation(
      this._visitorId,
      experienceKey,
      {
        ...attributes,
        visitorProperties, // represents audiences
        environment: attributes?.environment || this._environment,
        // qs-02: while previewing, every OTHER experience still decides but is
        // fully suppressed from tracking/persisting (zero-trace, AC5).
        ...(this._preview ? {enableTracking: false, enableStorage: false} : {})
      }
    );
    if (Object.values(RuleError).includes(bucketedVariation as RuleError))
      return bucketedVariation as RuleError;
    if (
      Object.values(BucketingError).includes(
        bucketedVariation as BucketingError
      )
    )
      return bucketedVariation as BucketingError;
    if (bucketedVariation) {
      if (!this._preview) {
        this._eventManager.fire(
          SystemEvents.BUCKETING,
          {
            visitorId: this._visitorId,
            experienceKey,
            variationKey: (bucketedVariation as BucketedVariation).key
          },
          null,
          true
        );
      }
    }
    return bucketedVariation as BucketedVariation;
  }

  /**
   * Get variations across all experiences
   * @param {BucketingAttributes=} attributes An object that specifies attributes for the visitor
   * @param {string=} attributes.locationProperties An object of key-value pairs that are used for location matching
   * @param {Record<any, any>=} attributes.visitorProperties An object of key-value pairs that are used for audience targeting
   * @param {boolean=} attributes.updateVisitorProperties Decide whether to update visitor properties upon bucketing
   * @param {string=} attributes.environment Overwrite the environment
   * @return {Array<BucketedVariatio | RuleError | BucketingError>}
   */
  runExperiences(
    attributes?: BucketingAttributes
  ): Array<BucketedVariation | RuleError | BucketingError> {
    if (!this._visitorId) {
      this._loggerManager?.error?.(
        'Context.runExperiences()',
        ERROR_MESSAGES.VISITOR_ID_REQUIRED
      );
      return;
    }
    const visitorProperties = this.getVisitorProperties(
      attributes?.visitorProperties
    );
    const bucketedVariations = this._experienceManager.selectVariations(
      this._visitorId,
      {
        ...attributes,
        visitorProperties, // represents audiences
        environment: attributes?.environment || this._environment,
        // qs-02: suppress tracking/persisting while previewing (AC5).
        ...(this._preview ? {enableTracking: false, enableStorage: false} : {})
      }
    );
    // Return rule errors if present
    const matchedRuleErrors = bucketedVariations.filter((match) =>
      Object.values(RuleError).includes(match as RuleError)
    );
    if (matchedRuleErrors.length) return matchedRuleErrors as Array<RuleError>;
    // Return bucketing errors if present
    const matchedBucketingErrors = bucketedVariations.filter((match) =>
      Object.values(BucketingError).includes(match as BucketingError)
    );
    if (matchedBucketingErrors.length)
      return matchedBucketingErrors as Array<BucketingError>;

    if (!this._preview) {
      (bucketedVariations as Array<BucketedVariation>).forEach(
        ({experienceKey, key}) => {
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
        }
      );
      return bucketedVariations as Array<BucketedVariation>;
    }
    // qs-02: preview forcing is API-agnostic -- the previewed experience must
    // return its forced variation from run-all too, not only from
    // runExperience(key) (parity with the PHP/Android/Python/iOS SDKs).
    // Replace the previewed experience's normally-bucketed entry with the
    // forced decision, or append it when the previewed experience is a draft
    // absent from the run-all set (mirrors runExperience's getPreviewDecision
    // path). No BUCKETING event fires and nothing persists -- zero-trace (AC5)
    // is already guaranteed by the enableTracking/enableStorage:false forwarded
    // into selectVariations above.
    const preview = this._preview;
    const forced = this._dataManager.getPreviewDecision(
      preview.experience,
      preview.variationId
    );
    if (!forced) return bucketedVariations as Array<BucketedVariation>;
    const forcedRest = (bucketedVariations as Array<BucketedVariation>).filter(
      (variation) => variation.experienceKey !== preview.experienceKey
    );
    return [...forcedRest, forced];
  }

  /**
   * Get feature and its status
   * @param {string} key A feature key
   * @param {BucketingAttributes=} attributes An object that specifies attributes for the visitor
   * @param {string=} attributes.locationProperties An object of key-value pairs that are used for location matching
   * @param {Record<any, any>=} attributes.visitorProperties An object of key-value pairs that are used for audience targeting
   * @param {boolean=} attributes.updateVisitorProperties Decide whether to update visitor properties upon bucketing
   * @param {string=} attributes.environment Overwrite the environment
   * @param {boolean=} attributes.typeCasting Control automatic type conversion to the variable's defined type. Does not do any JSON validation. Defaults to `true`
   * @param {Array<string>=} attributes.experienceKeys Use only specific experiences
   * @return {BucketedFeature | RuleError | Array<BucketedFeature | RuleError>}
   */
  runFeature(
    key: string,
    attributes?: BucketingAttributes
  ): BucketedFeature | RuleError | Array<BucketedFeature | RuleError> {
    if (!this._visitorId) {
      this._loggerManager?.error?.(
        'Context.runFeature()',
        ERROR_MESSAGES.VISITOR_ID_REQUIRED
      );
      return;
    }
    const visitorProperties = this.getVisitorProperties(
      attributes?.visitorProperties
    );
    const bucketedFeature = this._featureManager.runFeature(
      this._visitorId,
      key,
      {
        visitorProperties,
        locationProperties: attributes?.locationProperties,
        updateVisitorProperties: attributes?.updateVisitorProperties,
        typeCasting: Object.prototype.hasOwnProperty.call(
          attributes || {},
          'typeCasting'
        )
          ? attributes.typeCasting
          : true,
        environment: attributes?.environment || this._environment,
        // qs-02: suppress tracking/persisting while previewing (AC5).
        ...(this._preview ? {enableTracking: false, enableStorage: false} : {})
      },
      attributes?.experienceKeys
    );
    if (Array.isArray(bucketedFeature)) {
      // Return rule errors if present
      const matchedErrors = bucketedFeature.filter((match) =>
        Object.values(RuleError).includes(match as RuleError)
      );
      if (matchedErrors.length) return matchedErrors as Array<RuleError>;

      if (!this._preview) {
        (bucketedFeature as Array<BucketedFeature>).forEach(
          ({experienceKey, status}) => {
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
          }
        );
      }
    } else {
      if (Object.values(RuleError).includes(bucketedFeature as RuleError))
        return bucketedFeature as RuleError;

      if (bucketedFeature && !this._preview) {
        this._eventManager.fire(
          SystemEvents.BUCKETING,
          {
            visitorId: this._visitorId,
            experienceKey: (bucketedFeature as BucketedFeature).experienceKey,
            featureKey: key,
            status: (bucketedFeature as BucketedFeature).status
          },
          null,
          true
        );
      }
    }
    return bucketedFeature as BucketedFeature;
  }

  /**
   * Get features and their statuses
   * @param {BucketingAttributes=} attributes An object that specifies attributes for the visitor
   * @param {string=} attributes.locationProperties An object of key-value pairs that are used for location matching
   * @param {Record<any, any>=} attributes.visitorProperties An object of key-value pairs that are used for audience targeting
   * @param {boolean=} attributes.updateVisitorProperties Decide whether to update visitor properties upon bucketing
   * @param {string=} attributes.environment Overwrite the environment
   * @param {boolean=} attributes.typeCasting Control automatic type conversion to the variable's defined type. Does not do any JSON validation. Defaults to `true`
   * @return {Array<BucketedFeature | RuleError>}
   */
  runFeatures(
    attributes?: BucketingAttributes
  ): Array<BucketedFeature | RuleError> {
    if (!this._visitorId) {
      this._loggerManager?.error?.(
        'Context.runFeatures()',
        ERROR_MESSAGES.VISITOR_ID_REQUIRED
      );
      return;
    }
    const visitorProperties = this.getVisitorProperties(
      attributes?.visitorProperties
    );
    const bucketedFeatures = this._featureManager.runFeatures(this._visitorId, {
      visitorProperties,
      locationProperties: attributes?.locationProperties,
      updateVisitorProperties: attributes?.updateVisitorProperties,
      typeCasting: Object.prototype.hasOwnProperty.call(
        attributes || {},
        'typeCasting'
      )
        ? attributes.typeCasting
        : true,
      environment: attributes?.environment || this._environment,
      // qs-02: suppress tracking/persisting while previewing (AC5).
      ...(this._preview ? {enableTracking: false, enableStorage: false} : {})
    });
    // Return rule errors if present
    const matchedErrors = bucketedFeatures.filter((match) =>
      Object.values(RuleError).includes(match as RuleError)
    );
    if (matchedErrors.length) return matchedErrors as Array<RuleError>;

    if (!this._preview) {
      (bucketedFeatures as Array<BucketedFeature>).forEach(
        ({experienceKey, key, status}) => {
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
        }
      );
    }
    return bucketedFeatures as Array<BucketedFeature>;
  }

  /**
   * Trigger Conversion
   * @param {string} goalKey A goal key
   * @param {ConversionAttributes=} attributes An object that specifies attributes for the visitor
   * @param {Record<string, any>=} attributes.ruleData An object of key-value pairs that are used for goal matching
   * @param {Array<GoalData>=} attributes.conversionData An array of key-value pairs that are used for transaction data
   * @param {Record<ConversionSettingKey, number | string | boolean>} attributes.conversionSetting An object of key-value pairs that are used for tracking settings
   * @return {RuleError}
   */
  trackConversion(
    goalKey: string,
    attributes?: ConversionAttributes
  ): RuleError {
    if (!this._visitorId) {
      this._loggerManager?.error?.(
        'Context.trackConversion()',
        ERROR_MESSAGES.VISITOR_ID_REQUIRED
      );
      return;
    }
    // qs-02: zero-trace -- a previewing context never converts (AC5). Return
    // before calling dataManager.convert() so no store write / enqueue can occur.
    if (this._preview) {
      this._loggerManager?.trace?.(
        'Context.trackConversion()',
        'Skipped: Context is in preview mode',
        {goalKey}
      );
      return;
    }

    const goalRule = attributes?.ruleData;
    const goalData = attributes?.conversionData;
    if (goalData) {
      if (!Array.isArray(goalData)) {
        this._loggerManager?.error?.(
          'Context.trackConversion()',
          ERROR_MESSAGES.GOAL_DATA_NOT_VALID
        );
        return;
      }
    }

    const segments = this._segmentsManager.getSegments(this._visitorId);
    const triggred = this._dataManager.convert(
      this._visitorId,
      goalKey,
      goalRule,
      goalData,
      segments,
      attributes?.conversionSetting
    );
    if (Object.values(RuleError).includes(triggred as RuleError))
      return triggred as RuleError;
    if (triggred) {
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

    return;
  }

  /**
   * Set default segments for reports
   * @param {VisitorSegments} segments A segment key
   */
  setDefaultSegments(segments: VisitorSegments): void {
    // qs-02: preview contexts must leave zero trace -- suppress the
    // persistence write while preserving normal-context behavior exactly.
    this._segmentsManager.putSegments(
      this._visitorId,
      segments,
      !this._preview
    );
  }

  /**
   * To be deprecated
   */
  setCustomSegments(
    segmentKeys: string[],
    attributes?: SegmentsAttributes
  ): RuleError {
    return this.runCustomSegments(segmentKeys, attributes);
  }

  /**
   * Match Custom segments
   * @param {Array<string>} segmentKeys A list of segment keys
   * @param {SegmentsAttributes=} attributes An object that specifies attributes for the visitor
   * @param {Record<string, any>=} attributes.ruleData An object of key-value pairs that are used for segments matching
   * @return {RuleError}
   */
  runCustomSegments(
    segmentKeys: Array<string>,
    attributes?: SegmentsAttributes
  ): RuleError {
    if (!this._visitorId) {
      this._loggerManager?.error?.(
        'Context.runCustomSegments()',
        ERROR_MESSAGES.VISITOR_ID_REQUIRED
      );
      return;
    }
    const segmentsRule = this.getVisitorProperties(attributes?.ruleData);
    // qs-02: preview contexts must leave zero trace -- suppress the
    // persistence write while preserving normal-context matching behavior.
    const error = this._segmentsManager.selectCustomSegments(
      this._visitorId,
      segmentKeys,
      segmentsRule,
      !this._preview
    );
    if (error) return error as RuleError;

    return;
  }

  /**
   * Update visitor properties in memory
   * @param {string} visitorId
   * @param {Record<string, any>} visitorProperties
   */
  updateVisitorProperties(
    visitorId: string,
    visitorProperties: Record<string, any>
  ): void {
    // qs-02: preview contexts must leave zero trace -- skip the persistence
    // write entirely (DataManager.putData() has no per-call storage gate).
    if (this._preview) return;
    this._dataManager.putData(visitorId, {segments: visitorProperties});
  }

  /**
   * get Config Entity
   * @param {string} key
   * @param {EntityType} entityType
   * @return {Entity}
   */
  getConfigEntity(key: string, entityType: EntityType): Entity {
    if (entityType === EntityType.VARIATION) {
      const experiences = this._dataManager.getEntitiesList(
        EntityType.EXPERIENCE
      ) as Array<ConfigExperience>;
      for (const {key: experienceKey} of experiences) {
        const variation = this._dataManager.getSubItem(
          'experiences',
          experienceKey,
          'variations',
          key,
          'key',
          'key'
        ) as ExperienceVariationConfig;
        if (variation) {
          return variation;
        }
      }
    }
    return this._dataManager.getEntity(key, entityType);
  }

  /**
   * get Config Entity by string
   * @param {string} id
   * @param {EntityType} entityType
   * @return {Entity}
   */
  getConfigEntityById(id: string, entityType: EntityType): Entity {
    if (entityType === EntityType.VARIATION) {
      const experiences = this._dataManager.getEntitiesList(
        EntityType.EXPERIENCE
      ) as Array<ConfigExperience>;
      for (const {id: experienceId} of experiences) {
        const variation = this._dataManager.getSubItem(
          'experiences',
          experienceId,
          'variations',
          id,
          'id',
          'id'
        ) as ExperienceVariationConfig;
        if (variation) {
          return variation;
        }
      }
    }
    return this._dataManager.getEntityById(id, entityType);
  }

  /**
   * Get visitor data
   * @returns {StoreData}
   */
  getVisitorData(): StoreData {
    return this._dataManager.getData(this._visitorId) || {};
  }

  /**
   * Send pending API/DataStore queues to server
   * @param {string=} reason
   * @return {Promise<any>}
   */
  releaseQueues(reason?: string): Promise<any> {
    if (this._dataManager.dataStoreManager)
      this._dataManager.dataStoreManager.releaseQueue(reason);
    return this._apiManager.releaseQueue(reason);
  }

  /**
   * Get visitor properties
   * @param {Record<string, any>=} attributes An object of key-value pairs that are used for audience targeting
   * @return {Record<string, any>}
   */
  private getVisitorProperties(
    attributes?: Record<string, any>
  ): Record<string, any> {
    const {segments} = this._dataManager.getData(this._visitorId) || {};
    const visitorProperties = attributes
      ? objectDeepMerge(this._visitorProperties || {}, attributes)
      : this._visitorProperties;
    return objectDeepMerge(segments || {}, visitorProperties || {});
  }
}
