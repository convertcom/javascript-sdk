/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {ApiManagerInterface} from '@convertcom/js-sdk-api';
import {ContextInterface} from './interfaces/context';
import {CoreInterface} from './interfaces/core';
import {DataManagerInterface} from '@convertcom/js-sdk-data';
import {EventManagerInterface} from '@convertcom/js-sdk-event';
import {ExperienceManagerInterface} from '@convertcom/js-sdk-experience';
import {FeatureManagerInterface} from './interfaces/feature-manager';
import {LogManagerInterface} from '@convertcom/js-sdk-logger';
import {SegmentsManagerInterface} from '@convertcom/js-sdk-segments';

import {Config, ConfigData, Id} from '@convertcom/js-sdk-types';

import {ERROR_MESSAGES, MESSAGES, SystemEvents} from '@convertcom/js-sdk-enums';
import {objectNotEmpty} from '@convertcom/js-sdk-utils';
import {Context} from './context';

const DEFAULT_DATA_REFRESH_INTERVAL = 300000; // in milliseconds (5 minutes)

/**
 * Core
 * @category Main
 * @constructor
 * @implements {CoreInterface}
 */
export class Core implements CoreInterface {
  private _dataManager: DataManagerInterface;
  private _eventManager: EventManagerInterface;
  private _experienceManager: ExperienceManagerInterface;
  private _featureManager: FeatureManagerInterface;
  private _segmentsManager: SegmentsManagerInterface;
  private _loggerManager: LogManagerInterface;
  private _apiManager: ApiManagerInterface;
  private _config: Config;
  private _promise: Promise<ConfigData>;
  private _fetchConfigTimerID: number;
  private _environment: string;
  private _initialized: boolean;

  /**
   * @param {Config} config
   * @param {DataManagerInterface} dependencies.dataManager
   * @param {EventManagerInterface} dependencies.eventManager
   * @param {ExperienceManagerInterface} dependencies.experienceManager
   * @param {FeatureManagerInterface} dependencies.featureManager
   * @param {SegmentsManagerInterface} dependencies.segmentsManager
   * @param {ApiManagerInterface} dependencies.apiManager
   * @param {LogManagerInterface} dependencies.loggerManager
   */
  constructor(
    config: Config,
    {
      dataManager,
      eventManager,
      experienceManager,
      featureManager,
      segmentsManager,
      apiManager,
      loggerManager
    }: {
      dataManager: DataManagerInterface;
      eventManager: EventManagerInterface;
      experienceManager: ExperienceManagerInterface;
      featureManager: FeatureManagerInterface;
      segmentsManager: SegmentsManagerInterface;
      apiManager: ApiManagerInterface;
      loggerManager?: LogManagerInterface;
    }
  ) {
    this._initialized = false;
    this._environment = config?.environment;
    this._dataManager = dataManager;
    this._eventManager = eventManager;
    this._experienceManager = experienceManager;
    this._featureManager = featureManager;
    this._loggerManager = loggerManager;
    this._segmentsManager = segmentsManager;
    this._dataManager = dataManager;
    this._eventManager = eventManager;
    this._apiManager = apiManager;
    this._loggerManager = loggerManager;
    this._loggerManager?.trace?.(MESSAGES.CORE_CONSTRUCTOR, this);
    this.initialize(config);
  }

  /**
   * Initialize credentials, configData etc..
   * @param config
   */
  private initialize(config: Config): void {
    if (!config) return;
    this._config = config;
    if (
      Object.prototype.hasOwnProperty.call(config, 'sdkKey') &&
      config.sdkKey?.length
    ) {
      // Get data by sdk key
      this.fetchConfig();
    } else if (Object.prototype.hasOwnProperty.call(config, 'data')) {
      this._eventManager.fire(SystemEvents.READY, null, null, true);
      this._dataManager.data = config.data;
      this._loggerManager?.trace?.(MESSAGES.CORE_INITIALIZED);
      this._initialized = true;
    } else {
      this._loggerManager?.error?.(ERROR_MESSAGES.SDK_OR_DATA_OBJECT_REQUIRED);
      this._eventManager.fire(
        SystemEvents.READY,
        {},
        new Error(ERROR_MESSAGES.SDK_OR_DATA_OBJECT_REQUIRED),
        true
      );
    }
  }

  /**
   * Create visitor context
   * @param {Id} visitorId A visitor id
   * @param {Record<string, any>=} visitorAttributes An object of key-value pairs that are used for audience and/or segments targeting
   * @return {ContextInterface | null}
   */
  createContext(
    visitorId: Id,
    visitorAttributes?: Record<string, any>
  ): ContextInterface | null {
    if (!this._initialized) return null;
    return new Context(
      this._config,
      visitorId,
      {
        eventManager: this._eventManager,
        experienceManager: this._experienceManager,
        featureManager: this._featureManager,
        segmentsManager: this._segmentsManager,
        dataManager: this._dataManager,
        loggerManager: this._loggerManager
      },
      visitorAttributes
    );
  }

  /**
   * Add event handler to event
   * @param {SystemEvents} event Event name
   * @param {function(args, err): void} fn A callback function which will be fired
   */
  on(event: SystemEvents, fn: (args?, err?) => void): void {
    this._eventManager.on(event, fn);
  }

  /**
   * Promisified ready event
   * @return {Promise<void>}
   */
  async onReady(): Promise<void> {
    await this._promise;
    return new Promise((resolve, reject) => {
      if (objectNotEmpty(this._dataManager.data)) {
        resolve();
      } else {
        reject(new Error(ERROR_MESSAGES.DATA_OBJECT_MISSING));
      }
    });
  }

  /**
   * Enable tracking
   */
  enableTracking(): void {
    this._apiManager.enableTracking();
  }

  /**
   * Fetch remote config data
   * @return {Promise<void>}
   */
  private async fetchConfig(): Promise<void> {
    this._promise = this._apiManager.getConfigByKey(this._config.sdkKey);
    try {
      const data: ConfigData = await this._promise;
      this._loggerManager?.trace?.('Core.fetchConfig()', {
        data
      });
      this._eventManager.fire(
        objectNotEmpty(this._dataManager.data)
          ? SystemEvents.CONFIG_UPDATED
          : SystemEvents.READY,
        null,
        null,
        true
      );
      if (objectNotEmpty(this._dataManager.data)) {
        this._loggerManager?.trace?.(MESSAGES.CONFIG_DATA_UPDATED);
      } else {
        this._loggerManager?.trace?.(MESSAGES.CORE_INITIALIZED);
        this._initialized = true;
      }
      this._dataManager.data = data;
      this._apiManager.setData(data);
    } catch (error) {
      this._loggerManager?.error?.('Core.fetchConfig()', {
        error: error.message
      });
    }
    // Update data periodically in background
    if (this._fetchConfigTimerID) {
      clearTimeout(this._fetchConfigTimerID);
    }
    this._fetchConfigTimerID = setTimeout(
      () => this.fetchConfig(),
      this._config?.dataRefreshInterval || DEFAULT_DATA_REFRESH_INTERVAL
    ) as any;
  }
}
