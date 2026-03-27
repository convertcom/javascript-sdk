import {ApiManager} from '@convertcom/js-sdk-api';
import {BucketingManager} from '@convertcom/js-sdk-bucketing';
import {DataManager} from '@convertcom/js-sdk-data';
import {EventManager} from '@convertcom/js-sdk-event';
import {ExperienceManager} from '@convertcom/js-sdk-experience';
import {LogManager} from '@convertcom/js-sdk-logger';
import {RuleManager} from '@convertcom/js-sdk-rules';
import {SegmentsManager} from '@convertcom/js-sdk-segments';
import {Config as ConvertConfig, VisitorSegments} from '@convertcom/js-sdk-types';
import {ERROR_MESSAGES} from '@convertcom/js-sdk-enums';
import {Config} from '../config';
import {Core} from '../core';
import {FeatureManager} from '../feature-manager';

export type ConvertStandaloneRuntime = {
  sdk: Core;
  config: ConvertConfig;
  apiManager: ApiManager;
  bucketingManager: BucketingManager;
  dataManager: DataManager;
  eventManager: EventManager;
  experienceManager: ExperienceManager;
  featureManager: FeatureManager;
  loggerManager: LogManager;
  ruleManager: RuleManager;
  segmentsManager: SegmentsManager;
  ready: Promise<void>;
};

type ConvertWindowScope = Window &
  typeof globalThis & {
    convert?: Record<string, any>;
  };

const getWindowScope = (): ConvertWindowScope | null =>
  typeof window === 'undefined' ? null : (window as ConvertWindowScope);

const isRecord = (value: unknown): value is Record<string, any> =>
  !!value && typeof value === 'object' && !Array.isArray(value);

const getBootstrap = (convert: Record<string, any>): Record<string, any> =>
  isRecord(convert.bootstrap) ? convert.bootstrap : {};

const getRandomVisitorId = (): string => {
  const scope = globalThis as Record<string, any>;
  if (typeof scope?.crypto?.randomUUID === 'function') {
    return scope.crypto.randomUUID();
  }
  return `visitor-${Date.now()}-${Math.random().toString(16).slice(2)}`;
};

const getRuntimeConfig = (convert: Record<string, any>): ConvertConfig =>
  (getBootstrap(convert).config || convert.config || {}) as ConvertConfig;

const getVisitorAttributes = (convert: Record<string, any>): Record<string, any> =>
  isRecord(getBootstrap(convert).visitorAttributes)
    ? getBootstrap(convert).visitorAttributes
    : isRecord(convert.visitor?.attributes)
      ? convert.visitor.attributes
      : isRecord(convert.visitorAttributes)
        ? convert.visitorAttributes
        : {};

const getRuleData = (
  convert: Record<string, any>,
  runtime: ConvertStandaloneRuntime
): Record<string, any> =>
  (getBootstrap(convert).ruleData ||
    getBootstrap(convert).locationProperties ||
    convert.ruleData ||
    convert.locationProperties ||
    runtime.config?.ruleDataProvider ||
    {}) as Record<string, any>;

const getDefaultSegments = (
  convert: Record<string, any>
): VisitorSegments | null =>
  (getBootstrap(convert).defaultSegments || convert.defaultSegments || null) as
    | VisitorSegments
    | null;

const getVisitorId = (convert: Record<string, any>): string =>
  String(
    getBootstrap(convert).visitorId ||
      convert.visitor?.id ||
      convert.visitorId ||
      getRandomVisitorId()
  );

export const ensureConvertWindow = (): Record<string, any> => {
  const scope = getWindowScope();
  if (!scope) return {};
  scope.convert = scope.convert || {};
  return scope.convert;
};

export const createConvertRuntime = (
  rawConfig: ConvertConfig = {} as ConvertConfig
): ConvertStandaloneRuntime => {
  const isValidSDKKey = Boolean(
    Object.prototype.hasOwnProperty.call(rawConfig, 'sdkKey') &&
      rawConfig.sdkKey?.length
  );
  const isValidData = Boolean(
    Object.prototype.hasOwnProperty.call(rawConfig, 'data')
  );
  if (!isValidSDKKey && !isValidData) {
    console.error(ERROR_MESSAGES.SDK_OR_DATA_OBJECT_REQUIRED);
  }

  const configuration = Config(rawConfig);
  if (!configuration?.network) configuration.network = {};
  if (!configuration.network?.source) {
    configuration.network.source =
      typeof process.env.VERSION === 'string'
        ? process.env.VERSION
        : 'js-sdk';
  }

  const loggerManager = new LogManager(console, configuration.logger.logLevel);
  for (const key in configuration.logger.customLoggers) {
    if (
      Object.prototype.hasOwnProperty.call(
        configuration.logger.customLoggers[key],
        'logger'
      ) &&
      Object.prototype.hasOwnProperty.call(
        configuration.logger.customLoggers[key],
        'logLevel'
      )
    ) {
      loggerManager.addClient(
        configuration.logger.customLoggers[key].logger,
        configuration.logger.customLoggers[key].logLevel,
        configuration.logger.customLoggers[key]?.methodsMap
      );
    } else {
      loggerManager.addClient(
        configuration.logger.customLoggers[key],
        configuration.logger.logLevel
      );
    }
  }

  const eventManager = new EventManager(configuration, {loggerManager});
  const apiManager = new ApiManager(configuration, {eventManager, loggerManager});
  const bucketingManager = new BucketingManager(configuration, {loggerManager});
  const ruleManager = new RuleManager(configuration, {loggerManager});
  const dataManager = new DataManager(configuration, {
    bucketingManager,
    ruleManager,
    eventManager,
    apiManager,
    loggerManager
  });
  const experienceManager = new ExperienceManager(configuration, {dataManager});
  const featureManager = new FeatureManager(configuration, {
    dataManager,
    loggerManager
  });
  const segmentsManager = new SegmentsManager(configuration, {
    dataManager,
    ruleManager,
    loggerManager
  });
  const sdk = new Core(configuration, {
    dataManager,
    eventManager,
    experienceManager,
    featureManager,
    segmentsManager,
    apiManager,
    loggerManager
  });

  return {
    sdk,
    config: configuration,
    apiManager,
    bucketingManager,
    dataManager,
    eventManager,
    experienceManager,
    featureManager,
    loggerManager,
    ruleManager,
    segmentsManager,
    ready: sdk.onReady().catch(() => undefined)
  };
};

export const initializeVisitorRuntime = async (): Promise<ConvertStandaloneRuntime | null> => {
  const scope = getWindowScope();
  if (!scope) return null;

  const convert = ensureConvertWindow();
  const runtime = (isRecord(convert.remote) &&
  convert.remote.sdk &&
  convert.remote.dataManager
    ? convert.remote
    : createConvertRuntime(getRuntimeConfig(convert))) as ConvertStandaloneRuntime;

  convert.remote = runtime;
  convert.request = runtime.apiManager;
  convert.segments = runtime.segmentsManager;
  convert.dataStore = runtime.dataManager.dataStoreManager;
  convert.config = runtime.config;
  convert.ready = runtime.ready;

  await runtime.ready;

  convert.data = runtime.dataManager.data;
  convert.ruleData = getRuleData(convert, runtime);

  const visitorId = getVisitorId(convert);
  const visitorAttributes = getVisitorAttributes(convert);
  const context = runtime.sdk.createContext(
    visitorId,
    Object.keys(visitorAttributes).length ? visitorAttributes : undefined
  );

  convert.visitor = {
    ...(isRecord(convert.visitor) ? convert.visitor : {}),
    id: visitorId,
    attributes: visitorAttributes,
    context
  };

  const defaultSegments = getDefaultSegments(convert);
  if (context && defaultSegments) {
    context.setDefaultSegments(defaultSegments);
  }

  const locations = runtime.dataManager.getEntitiesList('locations') as Array<
    Record<string, any>
  >;
  if (context && Array.isArray(locations) && locations.length) {
    runtime.dataManager.selectLocations(visitorId, locations, {
      locationProperties: convert.ruleData,
      identityField: 'id'
    });
    convert.activeLocations = context.getVisitorData()?.locations || [];
  } else if (!Array.isArray(convert.activeLocations)) {
    convert.activeLocations = [];
  }

  return runtime;
};
