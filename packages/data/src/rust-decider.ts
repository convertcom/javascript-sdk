/*!
 * Shared helpers for calling the Rust decision engine (`@convertcom/decisions-core`).
 */
import type {
  ConfigResponseData,
  ExperienceVariationConfig
} from '@convertcom/js-sdk-types';
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

export class RustDeciderNotReadyError extends Error {
  constructor(message = 'Rust decision engine not ready') {
    super(message);
    this.name = 'RustDeciderNotReadyError';
  }
}

type DecisionsCoreModule = RustDecisionEngineNamespace & {
  default?: (...args: Array<unknown>) => unknown;
};

let moduleNamespace: RustDecisionEngineNamespace | null = null;
let initialized = false;
let initializationPromise: Promise<void> | null = null;

const loadModule = async (): Promise<RustDecisionEngineNamespace> => {
  const candidate = decisionsCoreModule as unknown as DecisionsCoreModule;

  if (typeof candidate.default === 'function') {
    try {
      const maybeNamespace = await candidate.default();
      if (
        maybeNamespace &&
        typeof maybeNamespace === 'object' &&
        'WasmDecisionEngine' in (maybeNamespace as unknown as Record<string, unknown>)
      ) {
        return maybeNamespace as RustDecisionEngineNamespace;
      }
    } catch (error) {
      // If initialization fails (e.g., missing fetch in legacy browsers), fall back to the raw module
    }
  }

  return candidate as RustDecisionEngineNamespace;
};

const setNamespace = (namespace: RustDecisionEngineNamespace | null) => {
  moduleNamespace = namespace;
  initialized = Boolean(namespace && namespace.WasmDecisionEngine);
};

export const initializeRustDecider = async (): Promise<void> => {
  if (initialized) return;
  if (!initializationPromise) {
    initializationPromise = (async () => {
      try {
        const namespace = await loadModule();
        setNamespace(namespace);
      } catch (error) {
        setNamespace(null);
        throw error;
      }
    })();
  }

  try {
    await initializationPromise;
  } finally {
    initializationPromise = null;
  }
};

export const isRustDeciderReady = (): boolean => initialized;

const getNamespace = (): RustDecisionEngineNamespace => {
  if (!moduleNamespace || !initialized) {
    throw new RustDeciderNotReadyError();
  }
  return moduleNamespace;
};

const disposeEngine = (engine: { free?: () => void }) => {
  try {
    engine?.free?.();
  } catch (error) {
    // ignore disposal issues
  }
};

export const decideExperienceWithRust = (
  project: ConfigResponseData['project'],
  request: RustExperienceDecisionRequest
): RustDecisionResponsePayload => {
  const namespace = getNamespace();
  const engine = new namespace.WasmDecisionEngine();
  try {
    return engine.decideExperience(project, request) as RustDecisionResponsePayload;
  } finally {
    disposeEngine(engine);
  }
};

export const aggregateFeaturesWithRust = (
  project: ConfigResponseData['project'],
  input: RustFeatureAggregationRequest
): RustFeatureAggregationPayload => {
  const namespace = getNamespace();
  const engine = new namespace.WasmDecisionEngine();
  try {
    return engine.aggregateFeatures(project, input) as RustFeatureAggregationPayload;
  } finally {
    disposeEngine(engine);
  }
};
