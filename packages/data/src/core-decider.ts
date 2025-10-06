/*!
 * Shared helpers for calling the Core decision engine (`@convertcom/decisions-core`).
 */
import type {
  ConfigResponseData,
  ExperienceVariationConfig
} from '@convertcom/js-sdk-types';
import type {LogManagerInterface} from '@convertcom/js-sdk-logger';
import * as decisionsCoreModule from '@convertcom/decisions-core';

export type RustSelectionType = 'forced' | 'cached' | 'newly_bucketed';
export type RustDecisionOutcomeType = 'matched' | 'not_matched';
export type RustLocationTransitionType = 'activated' | 'deactivated';
export type RustTrackingType = 'none' | 'bucket';

export interface RustVariationSummary {
  experience_id: string;
  experience_key: string;
  variation: ExperienceVariationConfig;
  allocation?: [number, number];
}

export interface RustSelectionPayload {
  type: RustSelectionType;
  summary: RustVariationSummary;
}

export interface RustMatchedOutcome {
  type: 'matched';
  selection: RustSelectionPayload;
}

export interface RustNotMatchedOutcome {
  type: 'not_matched';
  reason: string;
}

export type RustDecisionOutcome = RustMatchedOutcome | RustNotMatchedOutcome;

export interface RustStateDiff {
  bucketing: Record<string, string>;
  active_locations?: Array<string>;
  custom_segments?: Array<string>;
}

export interface RustLocationTransition {
  type: RustLocationTransitionType;
  location_id: string;
  location_key?: string;
  location_name?: string;
}

export interface RustLogRecord {
  level: string;
  message: string;
  data?: unknown;
}

export interface RustTrackingInstructionBucket {
  type: 'bucket';
  experience_id: string;
  variation_id: string;
}

export interface RustTrackingInstructionNone {
  type: 'none';
}

export type RustTrackingInstruction =
  | RustTrackingInstructionBucket
  | RustTrackingInstructionNone;

export interface RustDecisionResponsePayload {
  api_version: string;
  outcome: RustDecisionOutcome;
  state_diff: RustStateDiff;
  location_transitions: Array<RustLocationTransition>;
  matched_audience_ids: Array<string>;
  matched_segment_ids: Array<string>;
  logs: Array<RustLogRecord>;
  tracking: RustTrackingInstruction;
}

export interface RustFeatureAggregationPayload {
  api_version: string;
  features: Array<unknown>;
  logs: Array<RustLogRecord>;
}

export interface RustExperienceDecisionRequest {
  visitorId: string;
  experienceKey?: string;
  experienceId?: string;
  context: Record<string, unknown>;
  environment?: string;
  options?: Record<string, unknown>;
  visitorState?: Record<string, unknown>;
}

export interface RustFeatureAggregationRequest {
  variationSummaries: Array<RustVariationSummary>;
  filters?: Record<string, unknown>;
  typeCasting?: boolean;
}

export interface RustDecisionEngineNamespace {
  WasmDecisionEngine: new () => {
    decideExperience(
      project: ConfigResponseData['project'],
      request: RustExperienceDecisionRequest
    ): RustDecisionResponsePayload;
    aggregateFeatures(
      project: ConfigResponseData['project'],
      input: RustFeatureAggregationRequest
    ): RustFeatureAggregationPayload;
    free(): void;
  };
  default?: () => Promise<RustDecisionEngineNamespace>;
}

export class CoreDeciderNotReadyError extends Error {
  constructor(message = 'Core decision engine not ready') {
    super(message);
    this.name = 'CoreDeciderNotReadyError';
  }
}

type DecisionsCoreModule = RustDecisionEngineNamespace & {
  default?: (...args: Array<unknown>) => unknown;
};

/**
 * CoreDecider class manages the Core WASM decision engine lifecycle
 */
export class CoreDecider {
  private _loggerManager: LogManagerInterface | null;
  private _moduleNamespace: RustDecisionEngineNamespace | null = null;
  private _initialized = false;
  private _initializationPromise: Promise<void> | null = null;

  constructor({loggerManager}: {loggerManager?: LogManagerInterface} = {}) {
    this._loggerManager = loggerManager || null;
  }

  private async loadModule(): Promise<RustDecisionEngineNamespace> {
    this._loggerManager?.trace?.('CoreDecider.loadModule()', 'Attempting to load WASM module');
    const candidate = decisionsCoreModule as unknown as DecisionsCoreModule;

    if (typeof candidate.default === 'function') {
      this._loggerManager?.trace?.('CoreDecider.loadModule()', 'Found default export function, calling for async WASM init');
      try {
        const maybeNamespace = await candidate.default();

        if (
          maybeNamespace &&
          typeof maybeNamespace === 'object' &&
          'WasmDecisionEngine' in (maybeNamespace as unknown as Record<string, unknown>)
        ) {
          this._loggerManager?.trace?.('CoreDecider.loadModule()', 'Successfully loaded WASM namespace with WasmDecisionEngine');
          return maybeNamespace as RustDecisionEngineNamespace;
        }
      } catch (error) {
        this._loggerManager?.trace?.('CoreDecider.loadModule()', 'Async WASM init failed, falling back to raw module', {error});
        // If initialization fails (e.g., missing fetch in legacy browsers), fall back to the raw module
      }
    }

    this._loggerManager?.trace?.('CoreDecider.loadModule()', 'Using raw candidate as namespace');
    return candidate as RustDecisionEngineNamespace;
  }

  private setNamespace(namespace: RustDecisionEngineNamespace | null): void {
    this._moduleNamespace = namespace;
    this._initialized = Boolean(namespace && namespace.WasmDecisionEngine);
    this._loggerManager?.trace?.('CoreDecider.setNamespace()', 'Namespace set', {initialized: this._initialized});
  }

  async initialize(): Promise<void> {
    this._loggerManager?.trace?.('CoreDecider.initialize()', 'Called', {initialized: this._initialized});
    if (this._initialized) {
      this._loggerManager?.trace?.('CoreDecider.initialize()', 'Already initialized, skipping');
      return;
    }
    if (!this._initializationPromise) {
      this._loggerManager?.trace?.('CoreDecider.initialize()', 'Starting new initialization');
      this._initializationPromise = (async () => {
        try {
          const namespace = await this.loadModule();
          this.setNamespace(namespace);
          this._loggerManager?.trace?.('CoreDecider.initialize()', 'Initialization completed successfully');
        } catch (error) {
          this._loggerManager?.error?.('CoreDecider.initialize()', 'Initialization failed', {error});
          this.setNamespace(null);
          throw error;
        }
      })();
    }

    try {
      await this._initializationPromise;
    } finally {
      this._initializationPromise = null;
    }
  }

  isReady(): boolean {
    this._loggerManager?.trace?.('CoreDecider.isReady()', {initialized: this._initialized});
    return this._initialized;
  }

  private getNamespace(): RustDecisionEngineNamespace {
    this._loggerManager?.trace?.('CoreDecider.getNamespace()', 'Retrieving namespace', {
      initialized: this._initialized,
      hasNamespace: !!this._moduleNamespace
    });
    if (!this._moduleNamespace || !this._initialized) {
      this._loggerManager?.error?.('CoreDecider.getNamespace()', 'Namespace not ready');
      throw new CoreDeciderNotReadyError();
    }
    return this._moduleNamespace;
  }

  private disposeEngine(engine: {free?: () => void}): void {
    try {
      engine?.free?.();
    } catch (error) {
      // ignore disposal issues
    }
  }

  decideExperience(
    project: ConfigResponseData['project'],
    request: RustExperienceDecisionRequest
  ): RustDecisionResponsePayload {
    this._loggerManager?.info?.('CoreDecider.decideExperience()', '🦀 USING CORE DECIDER FOR EXPERIENCE', {
      experienceKey: request.experienceKey,
      experienceId: request.experienceId,
      visitorId: request.visitorId
    });
    const namespace = this.getNamespace();
    const engine = new namespace.WasmDecisionEngine();
    try {
      const result = engine.decideExperience(project, request) as RustDecisionResponsePayload;
      this._loggerManager?.info?.('CoreDecider.decideExperience()', '🦀 CORE: Experience decision completed', {
        outcomeType: result.outcome.type
      });
      return result;
    } finally {
      this.disposeEngine(engine);
    }
  }

  aggregateFeatures(
    project: ConfigResponseData['project'],
    input: RustFeatureAggregationRequest
  ): RustFeatureAggregationPayload {
    this._loggerManager?.info?.('CoreDecider.aggregateFeatures()', '🦀 USING CORE FOR FEATURE AGGREGATION', {
      variationCount: input.variationSummaries?.length,
      typeCasting: input.typeCasting
    });
    const namespace = this.getNamespace();
    const engine = new namespace.WasmDecisionEngine();
    try {
      const result = engine.aggregateFeatures(project, input) as RustFeatureAggregationPayload;
      this._loggerManager?.info?.('CoreDecider.aggregateFeatures()', '🦀 CORE: Feature aggregation completed', {
        featureCount: result.features?.length
      });
      return result;
    } finally {
      this.disposeEngine(engine);
    }
  }
}
