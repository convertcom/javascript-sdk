/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {objectDeepMerge} from '@convertcom/js-sdk-utils';

import {DataStoreManagerInterface} from './interfaces/data-store-manager';
import {LogManagerInterface} from '@convertcom/js-sdk-logger';
import {EventManagerInterface} from '@convertcom/js-sdk-event';
import {Config} from '@convertcom/js-sdk-types';
import {SystemEvents} from '@convertcom/js-sdk-enums';

import {ERROR_MESSAGES} from '@convertcom/js-sdk-enums';

const DEFAULT_BATCH_SIZE = 1;
const DEFAULT_RELEASE_INTERVAL = 5000;

/**
 * Data Store wrapper
 * @category Modules
 * @constructor
 * @implements {DataStoreManagerInterface}
 */
export class DataStoreManager implements DataStoreManagerInterface {
  private _requestsQueue: Record<string, any>;
  private _requestsQueueTimerID: number;

  private _loggerManager: LogManagerInterface | null;
  private _eventManager: EventManagerInterface | null;

  readonly batchSize: number = DEFAULT_BATCH_SIZE;
  readonly releaseInterval: number = DEFAULT_RELEASE_INTERVAL;

  private _dataStore;
  private _mapper: (...args: any) => any;

  /**
   * @param {Config=} config
   * @param {Object=} dependencies
   * @param {Object=} dependencies.dataStore
   * @param {EventManagerInterface=} dependencies.eventManager
   * @param {LogManagerInterface=} dependencies.loggerManager
   */
  constructor(
    config?: Config,
    {
      dataStore,
      eventManager,
      loggerManager
    }: {
      dataStore?;
      eventManager?: EventManagerInterface;
      loggerManager?: LogManagerInterface;
    } = {}
  ) {
    this._loggerManager = loggerManager;
    this._eventManager = eventManager;

    // TODO: Make this be configurable by config
    this.batchSize =
      // Number(objectDeepValue(config, 'events.batch_size')).valueOf() ||
      DEFAULT_BATCH_SIZE;

    this.releaseInterval =
      // Number(objectDeepValue(config, 'events.release_interval')).valueOf() ||
      DEFAULT_RELEASE_INTERVAL;
    this.dataStore = dataStore;
    this._mapper = config?.mapper || ((value: any) => value);
    this._requestsQueue = {};
  }

  set(key: string, data: any) {
    try {
      this.dataStore?.set?.(key, data);
    } catch (error) {
      this._loggerManager?.error?.('DataStoreManager.set()', {
        error: error.message
      });
    }
  }

  get(key: string): any {
    try {
      return this.dataStore?.get?.(key);
    } catch (error) {
      this._loggerManager?.error?.('DataStoreManager.get()', {
        error: error.message
      });
    }
    return null;
  }

  enqueue(key: string, data: any) {
    this._loggerManager?.trace?.(
      'DataStoreManager.enqueue()',
      this._mapper({
        key: key,
        data: data
      })
    );
    const addData = {};
    addData[key] = data;
    this._requestsQueue = objectDeepMerge(this._requestsQueue, addData);
    if (Object.keys(this._requestsQueue).length >= this.batchSize) {
      this.releaseQueue('size');
    } else {
      if (Object.keys(this._requestsQueue).length === 1) {
        this.startQueue();
      }
    }
  }

  releaseQueue(reason?: string): any {
    this._loggerManager?.info?.('DataStoreManager.releaseQueue()', {
      reason: reason || ''
    });
    this.stopQueue();
    for (const key in this._requestsQueue) {
      this.set(key, this._requestsQueue[key]);
    }
    this._eventManager?.fire?.(SystemEvents.DATA_STORE_QUEUE_RELEASED, {
      reason: reason || ''
    });
  }

  stopQueue() {
    clearTimeout(this._requestsQueueTimerID);
  }

  startQueue() {
    this._requestsQueueTimerID = setTimeout(() => {
      this.releaseQueue('timeout');
    }, this.releaseInterval) as any;
  }

  /**
   * dataStore setter
   * @param {any=} dataStore
   */
  set dataStore(dataStore: any) {
    if (dataStore) {
      if (this.isValidDataStore(dataStore)) {
        this._dataStore = dataStore;
      } else {
        this._loggerManager?.error?.(
          'DataStoreManager.dataStore.set()',
          ERROR_MESSAGES.DATA_STORE_NOT_VALID
        );
      }
    }
  }

  /**
   * dataStore getter
   */
  get dataStore() {
    return this._dataStore;
  }

  /**
   * Validates dataStore object
   * @param {any=} dataStore
   * @return {boolean}
   */
  isValidDataStore(dataStore: any): boolean {
    return (
      typeof dataStore === 'object' &&
      typeof dataStore['get'] === 'function' &&
      typeof dataStore['set'] === 'function'
    );
  }
}
