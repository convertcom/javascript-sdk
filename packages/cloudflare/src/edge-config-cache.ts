/*!
 * Convert JS SDK - Cloudflare Workers Utilities
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

const DEFAULT_CONFIG_ENDPOINT = 'https://cdn-4.convertexperiments.com/api/v1';

/**
 * Caches the Convert SDK configuration using Cloudflare's built-in fetch cache.
 *
 * Instead of requiring a KV namespace, this leverages the `cf` option on
 * `fetch()` to cache the config response at the edge. This is simpler,
 * works on all Cloudflare plans, and avoids KV read/write costs.
 *
 * @see https://developers.cloudflare.com/workers/examples/cache-using-fetch/
 *
 * @example
 * ```typescript
 * const configCache = new EdgeConfigCache('YOUR_SDK_KEY');
 * const configData = await configCache.getConfig();
 *
 * const sdk = new ConvertSDK({ data: configData });
 * ```
 */
export class EdgeConfigCache {
  private _sdkKey: string;
  private _ttl: number;
  private _configEndpoint: string;

  /**
   * @param sdkKey - Your Convert SDK key (e.g. 'ACCOUNT_ID/PROJECT_ID')
   * @param ttl - Cache TTL in seconds (default: 300 = 5 minutes)
   * @param configEndpoint - Override the config CDN endpoint
   */
  constructor(sdkKey: string, ttl = 300, configEndpoint?: string) {
    this._sdkKey = sdkKey;
    this._ttl = ttl;
    this._configEndpoint = configEndpoint || DEFAULT_CONFIG_ENDPOINT;
  }

  /**
   * Get the SDK configuration, served from Cloudflare's edge cache when
   * available. Falls back to fetching from the Convert CDN if the cache
   * entry has expired.
   */
  async getConfig(): Promise<any> {
    return this._fetch(this._ttl);
  }

  /**
   * Force-refresh the configuration by bypassing the edge cache.
   * Use this for manual cache invalidation (e.g. via a cron trigger or webhook).
   */
  async refreshConfig(): Promise<any> {
    return this._fetch(0);
  }

  private async _fetch(cacheTtl: number): Promise<any> {
    const url = `${this._configEndpoint}/config/${this._sdkKey}`;
    // The `cf` property is a Cloudflare Workers extension to the standard
    // fetch API that controls edge caching behaviour.
    const response = await fetch(url, {
      headers: {'Content-Type': 'application/json'},
      cf: {cacheTtl, cacheEverything: true}
    } as any);

    if (!response.ok) {
      throw new Error(
        `Convert config fetch failed: ${response.status} ${response.statusText}`
      );
    }

    return response.json();
  }
}
