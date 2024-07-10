/*!
 * Convert JS SDK test
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * Module dependencies
 */
import {ApiManager} from '@convertcom/js-sdk-api';
import {BucketingManager} from '@convertcom/js-sdk-bucketing';
import {Core} from './src/core';
import {CoreInterface} from './src/interfaces/core';
import {ContextInterface} from './src/interfaces/context';
import {
  DataManager,
  DataManagerInterface,
  DataStoreManagerInterface
} from '@convertcom/js-sdk-data';
import {EventManager, EventManagerInterface} from '@convertcom/js-sdk-event';
import {
  ExperienceManager,
  ExperienceManagerInterface
} from '@convertcom/js-sdk-experience';
import {FeatureManager} from './src/feature-manager';
import {FeatureManagerInterface} from './src/interfaces/feature-manager';
import {RuleManager, RuleManagerInterface} from '@convertcom/js-sdk-rules';
import {
  SegmentsManager,
  SegmentsManagerInterface
} from '@convertcom/js-sdk-segments';
import {LogManager, LogManagerInterface} from '@convertcom/js-sdk-logger';

import {Config} from './src/config';
import {Config as ConfigType} from '@convertcom/js-sdk-types';
import {ERROR_MESSAGES} from '@convertcom/js-sdk-enums';

//todo: add config attributes definition
/**
 * Create new Convert SDK instance
 * @category Main
 * @constructor
 */
class ConvertSDK extends Core {
  /**
   * @param {ConfigType} config
   */
  constructor(config: ConfigType = <ConfigType>{}) {
    // Validate config before assigning defaults
    const isValidSDKKey = Boolean(
      Object.prototype.hasOwnProperty.call(config, 'sdkKey') &&
        config.sdkKey?.length
    );
    const isValidData = Boolean(
      Object.prototype.hasOwnProperty.call(config, 'data')
    );
    if (!isValidSDKKey && !isValidData) {
      console.error(ERROR_MESSAGES.SDK_OR_DATA_OBJECT_REQUIRED);
    }

    const configuration = Config(config);
    if (!configuration?.network) configuration.network = {};
    if (!configuration.network?.source)
      configuration.network.source = process.env.VERSION || 'js-sdk';
    const loggerManager = new LogManager(
      console,
      configuration.logger.logLevel
    );
    for (const k in configuration.logger.customLoggers) {
      if (
        Object.prototype.hasOwnProperty.call(
          configuration.logger.customLoggers[k],
          'logger'
        ) &&
        Object.prototype.hasOwnProperty.call(
          configuration.logger.customLoggers[k],
          'logLevel'
        )
      ) {
        loggerManager.addClient(
          configuration.logger.customLoggers[k].logger,
          configuration.logger.customLoggers[k].logLevel,
          configuration.logger.customLoggers[k]?.methodsMap
        );
      } else {
        loggerManager.addClient(
          configuration.logger.customLoggers[k],
          configuration.logger.logLevel
        );
      }
    }
    const eventManager = new EventManager(configuration, {
      loggerManager
    });
    const apiManager = new ApiManager(configuration, {
      eventManager,
      loggerManager
    });
    const bucketingManager = new BucketingManager(configuration, {
      loggerManager
    });
    const ruleManager = new RuleManager(configuration, {loggerManager});
    const dataManager = new DataManager(configuration, {
      bucketingManager,
      ruleManager,
      eventManager,
      apiManager,
      loggerManager
    });
    const experienceManager = new ExperienceManager(configuration, {
      dataManager
    });
    const featureManager = new FeatureManager(configuration, {
      dataManager,
      loggerManager
    });
    const segmentsManager = new SegmentsManager(configuration, {
      dataManager,
      ruleManager,
      loggerManager
    });
    super(configuration, {
      dataManager,
      eventManager,
      experienceManager,
      featureManager,
      segmentsManager,
      apiManager,
      loggerManager
    });
  }

  /**
   * Promisified ready event
   * @return {Promise<void>}
   */
  onReady(): Promise<void> {
    return super.onReady();
  }
}

export default ConvertSDK;

export {
  ConfigResponseData,
  StoreData,
  BucketedFeature,
  BucketedVariation,
  BucketingAttributes,
  ConversionAttributes,
  LocationAttributes,
  Entity,
  IdentityField,
  SegmentsAttributes,
  VisitorSegments,
  ConfigExperience,
  ExperienceVariationConfig,
  ExperienceChange,
  ConfigFeature,
  ConfigProject,
  ConfigGoal,
  ConfigAudience,
  ConfigLocation,
  GoalData,
  RequireAtLeastOne,
  VisitorTrackingEvents,
  Path
} from '@convertcom/js-sdk-types';
export {
  EntityType,
  RuleError,
  BucketingError,
  GoalDataKey,
  LogLevel,
  SystemEvents,
  VariationChangeType
} from '@convertcom/js-sdk-enums';
export {FileLogger, DataStore} from '@convertcom/js-sdk-utils';

export {
  ConfigType as ConvertConfig,
  CoreInterface as ConvertInterface,
  ContextInterface,
  FeatureManagerInterface,
  DataManagerInterface,
  DataStoreManagerInterface,
  EventManagerInterface,
  ExperienceManagerInterface,
  RuleManagerInterface,
  SegmentsManagerInterface,
  LogManagerInterface
};
