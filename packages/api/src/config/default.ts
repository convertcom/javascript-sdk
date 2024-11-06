/*!
 * Convert JS SDK config
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

export const DEFAULT_CONFIG_ENDPOINT =
  typeof process.env.CONFIG_ENDPOINT === 'string' &&
  !!process.env.CONFIG_ENDPOINT
    ? process.env.CONFIG_ENDPOINT
    : 'https://cdn-4.convertexperiments.com/api/v1/';
export const DEFAULT_TRACK_ENDPOINT =
  typeof process.env.TRACK_ENDPOINT === 'string' && !!process.env.TRACK_ENDPOINT
    ? process.env.TRACK_ENDPOINT
    : 'https://[project_id].metrics.convertexperiments.com/v1/';
