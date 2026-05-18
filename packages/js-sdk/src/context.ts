/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {ContextInterface} from './interfaces/context';
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
  MESSAGES,
  RuleError,
  SystemEvents,
  VariationChangeType
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
  private _contentSecurityPolicyNonce?: string;
  // `undefined` = not yet resolved; once resolved (either from config or
  // DOM auto-detect) we cache to avoid re-querying the DOM on every change.
  private _cspNonceResolved: boolean = false;

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
    this._contentSecurityPolicyNonce = config?.contentSecurityPolicyNonce;
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
    const visitorProperties = this.getVisitorProperties(
      attributes?.visitorProperties
    );
    const bucketedVariation = this._experienceManager.selectVariation(
      this._visitorId,
      experienceKey,
      {
        visitorProperties, // represents audiences
        locationProperties: attributes?.locationProperties, // represents site_area/locations
        updateVisitorProperties: attributes?.updateVisitorProperties,
        experienceTypes: attributes?.experienceTypes,
        environment: attributes?.environment || this._environment
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
        visitorProperties, // represents audiences
        locationProperties: attributes?.locationProperties, // represents site_area/locations
        updateVisitorProperties: attributes?.updateVisitorProperties,
        experienceTypes: attributes?.experienceTypes,
        environment: attributes?.environment || this._environment
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
        experienceTypes: attributes?.experienceTypes,
        typeCasting: Object.prototype.hasOwnProperty.call(
          attributes || {},
          'typeCasting'
        )
          ? attributes.typeCasting
          : true,
        environment: attributes?.environment || this._environment
      },
      attributes?.experienceKeys
    );
    if (Array.isArray(bucketedFeature)) {
      // Return rule errors if present
      const matchedErrors = bucketedFeature.filter((match) =>
        Object.values(RuleError).includes(match as RuleError)
      );
      if (matchedErrors.length) return matchedErrors as Array<RuleError>;

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
    } else {
      if (Object.values(RuleError).includes(bucketedFeature as RuleError))
        return bucketedFeature as RuleError;

      if (bucketedFeature) {
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
      experienceTypes: attributes?.experienceTypes,
      typeCasting: Object.prototype.hasOwnProperty.call(
        attributes || {},
        'typeCasting'
      )
        ? attributes.typeCasting
        : true,
      environment: attributes?.environment || this._environment
    });
    // Return rule errors if present
    const matchedErrors = bucketedFeatures.filter((match) =>
      Object.values(RuleError).includes(match as RuleError)
    );
    if (matchedErrors.length) return matchedErrors as Array<RuleError>;

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
    return bucketedFeatures as Array<BucketedFeature>;
  }

  /**
   * Apply web variation changes (CSS, JS, custom JS) to the DOM.
   *
   * Execution order, matching the tracking script monolith:
   *   1. experience.global_css → <style>
   *   2. experience.global_js  → <script>
   *   3. For each change in variation.changes[]:
   *      a. change.data.css        → <style>
   *      b. change.data.js         → <script>  (defaultCode: Visual Editor
   *                                              generated, calls convert.T.*;
   *                                              customCode: user-written)
   *      c. change.data.custom_js  → <script>  (defaultCode only)
   *
   * Supported change types: `defaultCode`, `customCode`, `defaultCodeMultipage`.
   *
   * Skipped change types:
   *   - `fullStackFeature` — handled by `runFeature`.
   *   - `defaultRedirect`  — handled by the Split Bundle, which must run
   *     before this method.
   *   - `richStructure`    — selector-scoped DOM mutations not yet
   *     supported by this renderer; logged at debug level. Authors using
   *     richStructure should rely on the tracking-script monolith for
   *     application until the SDK gains a selector-aware queue.
   *
   * **Multipage caveat:** `defaultCodeMultipage.data.page_id` identifies
   * which funnel step the change targets. This method applies the change
   * unconditionally — it is the caller's responsibility to invoke
   * `runVariation` only on the matching page. A debug log records the
   * page_id so the caller can correlate.
   *
   * Idempotent: tracks applied CSS/JS via DOM marker IDs, so re-calling with
   * the same variation is a no-op.
   *
   * Browser-only. No-ops with a logged warning in server environments.
   *
   * @param {BucketedVariation} bucketedVariation A variation returned from runExperience()
   * @param {Object=} options
   * @param {ConfigExperience=} options.experience Optional experience override (otherwise looked up by experienceKey)
   */
  runVariation(
    bucketedVariation: BucketedVariation,
    options?: {experience?: ConfigExperience}
  ): void {
    if (typeof document === 'undefined' || typeof window === 'undefined') {
      this._loggerManager?.warn?.(
        'Context.runVariation()',
        MESSAGES.RUN_VARIATION_BROWSER_ONLY
      );
      return;
    }
    if (!bucketedVariation) return;

    const experience =
      options?.experience ||
      (this.getConfigEntity(
        bucketedVariation.experienceKey,
        EntityType.EXPERIENCE
      ) as ConfigExperience);

    if (!experience) {
      this._loggerManager?.warn?.(
        'Context.runVariation()',
        MESSAGES.RUN_VARIATION_EXPERIENCE_NOT_FOUND.replace(
          '#',
          String(bucketedVariation.experienceKey)
        )
      );
    }

    // Fallback to 'unknown' if both ids are missing so marker IDs never
    // interpolate `undefined` (which would collide with any other change
    // that also produced an undefined id).
    const experienceId =
      experience?.id ?? bucketedVariation.experienceId ?? 'unknown';

    // Toolkit warning only fires for `defaultCode.data.js` — that field is
    // Visual Editor output that calls `convert.T.*`. `customCode.data.js`
    // is user-written and doesn't need the Toolkit; `richStructure.data.js`
    // is skipped entirely below.
    if (
      !(window as any).convert?.T &&
      Array.isArray(bucketedVariation.changes) &&
      bucketedVariation.changes.some(
        (c) =>
          c?.type === VariationChangeType.DEFAULT_CODE &&
          (c as {data?: {js?: string}})?.data?.js
      )
    ) {
      this._loggerManager?.warn?.(
        'Context.runVariation()',
        MESSAGES.RUN_VARIATION_TOOLKIT_MISSING
      );
    }

    if (experience?.global_css) {
      this._injectStyle(
        `conv-exp-${experienceId}-global-css`,
        experience.global_css
      );
    }
    if (experience?.global_js) {
      this._executeScript(
        `conv-exp-${experienceId}-global-js`,
        experience.global_js
      );
    }

    // Per-change marker IDs are scoped by experience + variation + change.
    // change.id is system-assigned and currently globally unique, but
    // scoping defensively guards against future ID-semantics changes and
    // against the case where two distinct configs are merged into one
    // page (e.g. previewing one experience while another is live).
    const variationId = bucketedVariation.id;
    for (const change of bucketedVariation.changes ?? []) {
      if (!change) continue;

      if (change.type === VariationChangeType.FULLSTACK_FEATURE) {
        this._loggerManager?.debug?.(
          'Context.runVariation()',
          MESSAGES.RUN_VARIATION_FULLSTACK_SKIPPED
        );
        continue;
      }
      if (change.type === VariationChangeType.DEFAULT_REDIRECT) {
        this._loggerManager?.warn?.(
          'Context.runVariation()',
          MESSAGES.RUN_VARIATION_REDIRECT_SKIPPED
        );
        continue;
      }
      if (change.type === VariationChangeType.RICH_STRUCTURE) {
        // richStructure changes are selector-scoped DOM mutations whose
        // `data` is a polymorphic key-value map (see
        // ExperienceChangeRichStructureDataBase in types.gen.ts).
        // Applying the embedded `data.js` blob in isolation would skip
        // the selector targeting the change depends on and could mutate
        // the wrong elements. Skip until the SDK supports a
        // selector-aware queue.
        this._loggerManager?.debug?.(
          'Context.runVariation()',
          MESSAGES.RUN_VARIATION_RICH_STRUCTURE_SKIPPED.replace(
            '#',
            String((change as {id?: string | number})?.id ?? '?')
          )
        );
        continue;
      }

      // Remaining supported change types: defaultCode, customCode,
      // defaultCodeMultipage — all share `data.{css?, js?, custom_js?}`.
      // ExperienceChangeServing is a discriminated union and TypeScript
      // can't narrow it via the negative `continue`s above, so we
      // restate the relevant subset structurally here.
      const data = (
        change as {
          data?: {
            css?: string | null;
            js?: string | null;
            custom_js?: string | null;
            page_id?: string;
          };
        }
      ).data;
      if (!data) continue;

      if (
        change.type === VariationChangeType.DEFAULT_CODE_MULTIPAGE &&
        data.page_id
      ) {
        // Caller is responsible for invoking runVariation only on the
        // matching funnel step (the SDK doesn't know the current URL
        // / page context). Log so the caller can correlate.
        this._loggerManager?.debug?.(
          'Context.runVariation()',
          MESSAGES.RUN_VARIATION_MULTIPAGE_PAGE.replace(
            '#',
            String((change as {id?: string | number})?.id ?? '?')
          ).replace('#', String(data.page_id))
        );
      }

      const markerPrefix = `conv-chg-${experienceId}-${variationId}-${change.id}`;
      if (data.css) {
        this._injectStyle(`${markerPrefix}-css`, data.css);
      }
      if (data.js) {
        this._executeScript(`${markerPrefix}-js`, data.js);
      }
      if (data.custom_js) {
        this._executeScript(`${markerPrefix}-custom-js`, data.custom_js);
      }
    }
  }

  /**
   * Resolve the CSP nonce to stamp on injected <style>/<script> elements.
   * Mirrors the tracking-script monolith's `getContentSecurityPolicyNonce()`
   * (workflow.ts in the backend repo):
   *   1. Prefer the explicit `Config.contentSecurityPolicyNonce` value
   *      captured at construction.
   *   2. Otherwise scan the live DOM for `<script>`/`<style>` elements
   *      carrying a nonce — read the IDL `.nonce` property because the
   *      browser blanks the HTML `nonce` attribute on these elements
   *      after they're connected (per the CSP spec). Fall back to
   *      `getAttribute('nonce')` only for non-script/style elements
   *      where the attribute is still visible.
   * Cached after first call so the DOM scan happens at most once per
   * Context instance. Skips empty-string nonces so an `<el nonce="">` on
   * the page doesn't poison the cache and cause every subsequent
   * injection to be tagged with an empty nonce (which the browser
   * would reject under a strict CSP).
   * @private
   */
  private _getCspNonce(): string | undefined {
    if (this._cspNonceResolved) return this._contentSecurityPolicyNonce;
    this._cspNonceResolved = true;
    if (this._contentSecurityPolicyNonce) {
      return this._contentSecurityPolicyNonce;
    }
    if (typeof document === 'undefined') return undefined;
    try {
      // Include `script` / `style` even without a `[nonce]` attribute:
      // once connected, the browser blanks the HTML nonce attribute on
      // these elements (per CSP spec) but preserves the IDL `.nonce`
      // property, so a plain `[nonce]` selector misses them. `[nonce]`
      // still catches nonces on arbitrary other elements where the
      // attribute is visible. querySelectorAll returns matches in
      // document-order and deduplicates, so the selector list reduces
      // to roughly `script, style, [nonce]` at runtime.
      const candidates = document.querySelectorAll('script, style, [nonce]');
      for (let i = 0; i < candidates.length; i++) {
        const el = candidates[i] as HTMLElement & {nonce?: string};
        const candidate =
          (typeof el.nonce === 'string' ? el.nonce : '') ||
          el.getAttribute('nonce') ||
          '';
        if (candidate) {
          this._contentSecurityPolicyNonce = candidate;
          return this._contentSecurityPolicyNonce;
        }
      }
    } catch (error) {
      this._loggerManager?.error?.(
        'Context.runVariation()',
        `Error reading nonce from DOM: ${(error as Error)?.message}`
      );
    }
    return this._contentSecurityPolicyNonce;
  }

  /**
   * Inject a <style> tag identified by markerId. No-op if already injected.
   * @private
   */
  private _injectStyle(markerId: string, css: string): void {
    try {
      if (document.getElementById(markerId)) return;
      const style = document.createElement('style');
      style.id = markerId;
      style.setAttribute('type', 'text/css');
      const nonce = this._getCspNonce();
      if (nonce) style.setAttribute('nonce', nonce);
      style.appendChild(document.createTextNode(css));
      (document.head || document.documentElement).appendChild(style);
    } catch (error) {
      this._loggerManager?.error?.(
        'Context.runVariation()',
        MESSAGES.RUN_VARIATION_STYLE_ERROR.replace('#', markerId),
        {message: (error as Error)?.message}
      );
    }
  }

  /**
   * Inject a <script> tag identified by markerId. No-op if already injected
   * (the script element with this id already exists in the DOM).
   * @private
   */
  private _executeScript(markerId: string, code: string): void {
    try {
      if (document.getElementById(markerId)) return;
      const script = document.createElement('script');
      script.id = markerId;
      script.setAttribute('type', 'text/javascript');
      const nonce = this._getCspNonce();
      if (nonce) script.setAttribute('nonce', nonce);
      script.appendChild(document.createTextNode(code));
      (document.head || document.documentElement).appendChild(script);
    } catch (error) {
      this._loggerManager?.error?.(
        'Context.runVariation()',
        MESSAGES.RUN_VARIATION_SCRIPT_ERROR.replace('#', markerId),
        {message: (error as Error)?.message}
      );
    }
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
    this._segmentsManager.putSegments(this._visitorId, segments);
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
    const error = this._segmentsManager.selectCustomSegments(
      this._visitorId,
      segmentKeys,
      segmentsRule
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
