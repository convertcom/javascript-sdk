/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * SDK system events. Possible values: 'ready' | 'queue-released'
 * or custom visitor's event
 */
export enum SystemEvents {
  READY = 'ready',
  CONFIG_UPDATED = 'config-updated',
  API_QUEUE_RELEASED = 'api-queue-released',
  BUCKETING = 'bucketing',
  CONVERSION = 'conversion',
  SEGMENTS = 'segments',
  LOCATION_ACTIVATED = 'location-activated',
  LOCATION_DEACTIVATED = 'location-deactivated',
  AUDIENCES = 'audiences',
  DATA_STORE_QUEUE_RELEASED = 'data-store-queue-released'
}
