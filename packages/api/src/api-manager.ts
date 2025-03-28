/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {ApiManagerInterface} from './interfaces/api-manager';

import {
  Config,
  ConfigResponseData,
  VisitorSegments,
  Visitor,
  VisitorsQueue,
  Path,
  TrackingEvent,
  VisitorTrackingEvents
} from '@convertcom/js-sdk-types';
import {MESSAGES, SystemEvents} from '@convertcom/js-sdk-enums';
import {LogManagerInterface} from '@convertcom/js-sdk-logger';
import {EventManagerInterface} from '@convertcom/js-sdk-event';
import {
  HttpClient,
  HttpMethod,
  HttpResponse,
  HttpResponseType,
  HttpRequest,
  objectDeepValue
} from '@convertcom/js-sdk-utils';

const DEFAULT_HEADERS = {
  'Content-Type': 'application/json'
};
const DEFAULT_BATCH_SIZE = 10;
const DEFAULT_RELEASE_INTERVAL = 10000;
const DEFAULT_CONFIG_ENDPOINT = process.env.CONFIG_ENDPOINT;
const DEFAULT_TRACK_ENDPOINT = process.env.TRACK_ENDPOINT;

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

  private _defaultHeaders: Record<string, string> = DEFAULT_HEADERS;
  private _data: ConfigResponseData;
  private _enrichData: boolean;
  private _environment: string;
  private _loggerManager: LogManagerInterface | null;
  private _eventManager: EventManagerInterface | null;
  private _sdkKey: string;
  private _accountId: string;
  private _projectId: string;
  private _trackingEvent: TrackingEvent;
  private _trackingEnabled: boolean;
  private _trackingSource: string;
  private _cacheLevel: string;
  private _mapper: (...args: any) => any;

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

    this._configEndpoint =
      config?.api?.endpoint?.config || DEFAULT_CONFIG_ENDPOINT;
    this._trackEndpoint =
      config?.api?.endpoint?.track || DEFAULT_TRACK_ENDPOINT;
    this._data = objectDeepValue(config, 'data');
    this._enrichData = !objectDeepValue(config, 'dataStore');
    this._environment = config?.environment;
    this._mapper = config?.mapper || ((value: any) => value);

    this.batchSize = Number(config?.events?.batch_size) || DEFAULT_BATCH_SIZE;

    this.releaseInterval =
      Number(config?.events?.release_interval) || DEFAULT_RELEASE_INTERVAL;

    this._accountId = this._data?.account_id;
    this._projectId = this._data?.project?.id;
    this._sdkKey = config?.sdkKey || `${this._accountId}/${this._projectId}`;
    if (config?.sdkKeySecret)
      this._defaultHeaders['Authorization'] = `Bearer ${config?.sdkKeySecret}`;
    this._trackingEvent = {
      enrichData: this._enrichData,
      accountId: this._accountId,
      projectId: this._projectId,
      visitors: []
    } as TrackingEvent;
    this._trackingEnabled = config?.network?.tracking;
    this._trackingSource = config?.network?.source || 'js-sdk';
    this._cacheLevel = config?.network?.cacheLevel;
    this._requestsQueue = {
      length: 0,
      items: [],
      push(
        visitorId: string,
        eventRequest: VisitorTrackingEvents,
        segments?: VisitorSegments
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
   * @param {string} visitorId
   * @param {VisitorTrackingEvents} eventRequest
   * @param {VisitorSegments} segments
   */
  enqueue(
    visitorId: string,
    eventRequest: VisitorTrackingEvents,
    segments?: VisitorSegments
  ): void {
    this._loggerManager?.trace?.(
      'ApiManager.enqueue()',
      this._mapper({
        eventRequest: eventRequest
      })
    );
    this._requestsQueue.push(visitorId, eventRequest, segments);
    if (this._trackingEnabled) {
      if (this._requestsQueue.length === 1) {
        this.startQueue();
      } else if (this._requestsQueue.length === this.batchSize) {
        this.releaseQueue('size').then();
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
    this._loggerManager?.info?.(
      'ApiManager.releaseQueue()',
      MESSAGES.RELEASING_QUEUE
    );
    this._loggerManager?.trace?.('ApiManager.releaseQueue()', {
      reason: reason || ''
    });
    this.stopQueue();
    const payload: TrackingEvent = this._trackingEvent;
    payload.visitors = this._requestsQueue.items.slice();
    payload.source = this._trackingSource;
    return this.request(
      'post',
      {
        base: this._trackEndpoint.replace(
          '[project_id]',
          this._projectId.toString()
        ),
        route: `/track/${this._sdkKey}`
      },
      this._mapper(payload)
    )
      .then((result) => {
        this._requestsQueue.reset();
        this._eventManager?.fire?.(SystemEvents.API_QUEUE_RELEASED, {
          reason: reason,
          result: result,
          visitors: payload.visitors
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
   * Enable tracking
   */
  enableTracking(): void {
    this._trackingEnabled = true;
    this.releaseQueue('trackingEnabled');
  }

  /**
   * Disable tracking
   */
  disableTracking(): void {
    this._trackingEnabled = false;
  }

  /**
   * Set data
   */
  setData(data: ConfigResponseData) {
    this._data = data;
    this._accountId = data?.account_id;
    this._projectId = data?.project?.id;
    this._trackingEvent.accountId = this._accountId;
    this._trackingEvent.projectId = this._projectId;
  }

  /**
   * Get config data
   * @return {Promise<ConfigResponseData>}
   */
  getConfig(): Promise<ConfigResponseData> {
    this._loggerManager?.trace?.('ApiManager.getConfig()');
    let query = this._cacheLevel === 'low' || this._environment ? '?' : '';
    if (this._environment) query += `environment=${this._environment}`;
    if (this._cacheLevel === 'low') query += '_conv_low_cache=1';
    return new Promise((resolve, reject) => {
      this.request('get', {
        base: this._configEndpoint,
        route: `/config/${this._sdkKey}${query}`
      })
        .then(({data}) => resolve(data))
        .catch(reject);
    });
  }
}
