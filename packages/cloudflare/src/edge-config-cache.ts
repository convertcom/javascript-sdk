/*!
 * Convert JS SDK - Cloudflare Workers Utilities
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * Minimal interface compatible with Cloudflare Workers KVNamespace.
 */
interface KVNamespaceLike {
  get(key: string): Promise<string | null>;
  put(
    key: string,
    value: string,
    options?: {expirationTtl?: number}
  ): Promise<void>;
}

const DEFAULT_CONFIG_ENDPOINT = 'https://cdn-4.convertexperiments.com/api/v1';

/**
 * Caches the Convert SDK configuration in Cloudflare KV.
 *
 * Instead of fetching config from the CDN on every Worker invocation,
 * this cache stores it in KV with a TTL. This reduces latency from
 * ~100ms (CDN round-trip) to ~1ms (edge KV read).
 *
 * @example
 * ```typescript
 * const configCache = new EdgeConfigCache(env.CONVERT_KV, 'YOUR_SDK_KEY');
 * const configData = await configCache.getConfig();
 *
 * const sdk = new ConvertSDK({ data: configData });
 * ```
 */
export class EdgeConfigCache {
  private _kv: KVNamespaceLike;
  private _sdkKey: string;
  private _ttl: number;
  private _configEndpoint: string;

  /**
   * @param kv - A Cloudflare KV namespace binding
   * @param sdkKey - Your Convert SDK key (e.g. 'ACCOUNT_ID/PROJECT_ID')
   * @param ttl - Cache TTL in seconds (default: 300 = 5 minutes)
   * @param configEndpoint - Override the config CDN endpoint
   */
  constructor(
    kv: KVNamespaceLike,
    sdkKey: string,
    ttl = 300,
    configEndpoint?: string
  ) {
    this._kv = kv;
    this._sdkKey = sdkKey;
    this._ttl = ttl;
    this._configEndpoint = configEndpoint || DEFAULT_CONFIG_ENDPOINT;
  }

  /**
   * Get the SDK configuration, serving from KV cache when available.
   * Falls back to fetching from the Convert CDN if cache is empty or expired.
   */
  async getConfig(): Promise<any> {
    const cacheKey = `config:${this._sdkKey}`;

    // Try KV cache first
    const cached = await this._kv.get(cacheKey);
    if (cached) return JSON.parse(cached);

    // Cache miss: fetch from CDN and store in KV
    return this._fetchAndCache(cacheKey);
  }

  /**
   * Force-refresh the configuration from the Convert CDN.
   * Use this for manual cache invalidation.
   */
  async refreshConfig(): Promise<any> {
    const cacheKey = `config:${this._sdkKey}`;
    return this._fetchAndCache(cacheKey);
  }

  private async _fetchAndCache(cacheKey: string): Promise<any> {
    const url = `${this._configEndpoint}/config/${this._sdkKey}`;
    const response = await fetch(url, {
      headers: {'Content-Type': 'application/json'}
    });

    if (!response.ok) {
      throw new Error(
        `Convert config fetch failed: ${response.status} ${response.statusText}`
      );
    }

    const data = await response.json();
    await this._kv.put(cacheKey, JSON.stringify(data), {
      expirationTtl: this._ttl
    });

    return data;
  }
}
