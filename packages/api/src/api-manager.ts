/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {ApiManagerInterface} from './interfaces/api-manager';

import {
  Config,
  ConfigData,
  Id,
  TrackingEvent,
  SegmentsData,
  Visitor,
  VisitorEvent,
  VisitorsQueue,
  Path
} from '@convertcom/js-sdk-types';
import {SystemEvents} from '@convertcom/js-sdk-enums';
import {objectDeepValue} from '@convertcom/js-sdk-utils';
import {LogManagerInterface} from '@convertcom/js-sdk-logger';
import {EventManagerInterface} from '@convertcom/js-sdk-event';
import {
  HttpClient,
  HttpMethod,
  HttpResponse,
  HttpResponseType,
  HttpRequest
} from '@convertcom/js-sdk-utils';

import {
  DEFAULT_CONFIG_ENDPOINT,
  DEFAULT_TRACK_ENDPOINT
} from './config/default';

const DEFAULT_HEADERS = {
  'Content-type': 'application/json'
};
const DEFAULT_BATCH_SIZE = 10;
const DEFAULT_RELEASE_INTERVAL = 10000;

/**
 * Provides logic for network requests. Reads remote configuration and sends tracking events to Convert server.
 * @category Modules
 * @constructor
 * @implements {ApiManagerInterface}
 */
export class ApiManager implements ApiManagerInterface {
  private _requestsQueue: VisitorsQueue;
  private _requestsQueueTimerID: number;

  private readonly _configEndpoint: string = DEFAULT_CONFIG_ENDPOINT;
  private readonly _trackEndpoint: string = DEFAULT_TRACK_ENDPOINT;
  private readonly _defaultHeaders: Record<string, string> = DEFAULT_HEADERS;

  private _data: ConfigData;
  private _enrichData: boolean;
  private _loggerManager: LogManagerInterface | null;
  private _eventManager: EventManagerInterface | null;
  private _accountId: Id;
  private _projectId: Id;
  private _trackingEvent: TrackingEvent;

  readonly batchSize: number = DEFAULT_BATCH_SIZE;
  readonly releaseInterval: number = DEFAULT_RELEASE_INTERVAL;

  /**
   * @param {Config=} config
   * @param {Object=} dependencies
   * @param {EventManagerInterface=} dependencies.eventManager
   * @param {LogManagerInterface=} dependencies.loggerManager
   */
  constructor(
    config?: Config,
    {
      eventManager,
      loggerManager
    }: {
      eventManager?: EventManagerInterface;
      loggerManager?: LogManagerInterface;
    } = {}
  ) {
    this._loggerManager = loggerManager;
    this._eventManager = eventManager;

    this._configEndpoint = objectDeepValue(
      config,
      'api.endpoint.config',
      DEFAULT_CONFIG_ENDPOINT
    );
    this._trackEndpoint = objectDeepValue(
      config,
      'api.endpoint.track',
      DEFAULT_TRACK_ENDPOINT
    );
    this._data = objectDeepValue(config, 'data');
    this._enrichData = !config?.dataStore;

    this.batchSize =
      Number(objectDeepValue(config, 'events.batch_size')).valueOf() ||
      DEFAULT_BATCH_SIZE;

    this.releaseInterval =
      Number(objectDeepValue(config, 'events.release_interval')).valueOf() ||
      DEFAULT_RELEASE_INTERVAL;

    this._accountId = this._data?.account_id;
    this._projectId = this._data?.project?.id;
    this._trackingEvent = {
      enrichData: this._enrichData,
      accountId: this._accountId,
      projectId: this._projectId,
      visitors: []
    };
    this._requestsQueue = {
      length: 0,
      items: [],
      push(
        visitorId: Id,
        eventRequest: VisitorEvent,
        segments?: SegmentsData
      ): void {
        const visitorIndex = this.items.findIndex(
          (event) => event.visitorId === visitorId
        );
        if (visitorIndex !== -1) {
          this.items[visitorIndex].events.push(eventRequest);
        } else {
          const visitor: Visitor = {
            visitorId,
            events: [eventRequest]
          };
          if (segments) visitor.segments = segments;
          this.items.push(visitor);
        }
        this.length++;
      },
      reset(): void {
        this.items = [];
        this.length = 0;
      }
    };
  }

  /**
   * Send request to api server
   * @param method
   * @param path
   * @param data
   * @param headers
   * @return {Promise<HttpResponse>}
   */
  async request(
    method: string,
    path: Path,
    data: Record<string, any> = {},
    headers: Record<string, any> = {}
  ): Promise<HttpResponse> {
    const requestHeaders = {
      ...this._defaultHeaders,
      ...headers
    };
    const requestConfig: HttpRequest = {
      method: <HttpMethod>method,
      path: path.route,
      baseURL: path.base,
      headers: requestHeaders,
      data: data,
      responseType: <HttpResponseType>'json'
    };
    return HttpClient.request(requestConfig);
  }

  /**
   * Add request to queue for sending to server
   * @param {Id} visitorId
   * @param {VisitorEvent} eventRequest
   * @param {SegmentsData} segments
   */
  enqueue(
    visitorId: Id,
    eventRequest: VisitorEvent,
    segments?: SegmentsData
  ): void {
    this._loggerManager?.trace?.('ApiManager.enqueue()', {
      eventRequest: eventRequest
    });
    this._requestsQueue.push(visitorId, eventRequest, segments);
    if (this._requestsQueue.length === this.batchSize) {
      this.releaseQueue('size').then();
    } else {
      if (this._requestsQueue.length === 1) {
        this.startQueue();
      }
    }
  }

  /**
   * Send queue to server
   * @param {string=} reason
   * @return {Promise<any>}
   */
  releaseQueue(reason?: string): Promise<any> {
    if (!this._requestsQueue.length) return;
    this._loggerManager?.trace?.('ApiManager.releaseQueue()', {
      reason: reason || ''
    });
    this.stopQueue();
    const payload: TrackingEvent = this._trackingEvent;
    payload.visitors = this._requestsQueue.items.slice();
    return this.request(
      'post',
      {
        base: this._trackEndpoint.replace(
          '[project_id]',
          this._projectId.toString()
        ),
        route: `/track/${this._accountId}/${this._projectId}`
      },
      payload
    )
      .then((result) => {
        this._requestsQueue.reset();
        this._eventManager?.fire?.(SystemEvents.API_QUEUE_RELEASED, {
          reason: reason,
          result: result
        });
      })
      .catch((error) => {
        // TODO: set an exponential backoff
        this._loggerManager?.error?.('ApiManager.releaseQueue()', {
          error: error.message
        });
        this.startQueue();
        this._eventManager?.fire?.(
          SystemEvents.API_QUEUE_RELEASED,
          {reason: reason},
          error
        );
      });
  }

  /**
   * Stop queue timer
   */
  stopQueue(): void {
    clearTimeout(this._requestsQueueTimerID);
  }

  /**
   * Start queue timer
   */
  startQueue(): void {
    this._requestsQueueTimerID = setTimeout(() => {
      this.releaseQueue('timeout');
    }, this.releaseInterval) as any;
  }

  /**
   * Set data
   */
  setData(data: ConfigData) {
    this._data = data;
    this._accountId = data?.account_id;
    this._projectId = data?.project?.id;
    this._trackingEvent.accountId = this._accountId;
    this._trackingEvent.projectId = this._projectId;
  }

  /**
   * Get config data by SDK key
   * @param {string} sdkKey
   * @return {Promise<ConfigData>}
   */
  getConfigByKey(sdkKey: string): Promise<ConfigData> {
    this._loggerManager?.trace?.('ApiManager.getConfigByKey()', {
      sdkKey
    });
    return new Promise((resolve, reject) => {
      this.request('get', {
        base: this._configEndpoint,
        route: `/config/${sdkKey}`
      })
        .then(({data}) => resolve(data))
        .catch(reject);
    });
  }
}
