/*!
 * Convert JS SDK - Cloudflare Workers Utilities
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * Minimal interface compatible with Cloudflare Workers KVNamespace.
 * Avoids a hard dependency on @cloudflare/workers-types.
 */
interface KVNamespaceLike {
  get(key: string): Promise<string | null>;
  put(
    key: string,
    value: string,
    options?: {expirationTtl?: number}
  ): Promise<void>;
}

/**
 * A DataStore adapter for Cloudflare Workers KV.
 *
 * The Convert SDK expects a synchronous DataStore (get/set).
 * Since KV is async, this adapter works in three phases:
 *   1. load()  - async read from KV into memory (call before SDK operations)
 *   2. get/set - sync read/write on in-memory snapshot (used by SDK)
 *   3. save()  - async write from memory back to KV (call after SDK operations)
 *
 * @example
 * ```typescript
 * const dataStore = new KVDataStore(env.CONVERT_KV);
 * await dataStore.load(visitorId);
 *
 * const sdk = new ConvertSDK({ data: configData, dataStore });
 * // ... run experiments ...
 *
 * await dataStore.save(visitorId);
 * ```
 */
export class KVDataStore {
  private _data: Record<string, any> = {};
  private _kv: KVNamespaceLike;
  private _dirty = false;
  private _prefix: string;

  /**
   * @param kv - A Cloudflare KV namespace binding
   * @param prefix - Key prefix for KV entries (default: 'visitor')
   */
  constructor(kv: KVNamespaceLike, prefix = 'visitor') {
    this._kv = kv;
    this._prefix = prefix;
  }

  /**
   * Load visitor data from KV into memory.
   * Call this BEFORE creating the SDK context.
   */
  async load(visitorId: string): Promise<void> {
    const raw = await this._kv.get(`${this._prefix}:${visitorId}`);
    try {
      this._data = raw ? JSON.parse(raw) : {};
    } catch (e) {
      // Corrupted data in KV, start fresh for this visitor.
      this._data = {};
    }
    this._dirty = false;
  }

  /**
   * Get a value by key (synchronous, reads from memory).
   * Called internally by the SDK.
   */
  get(key: string): any {
    return this._data[key];
  }

  /**
   * Set a value by key (synchronous, writes to memory).
   * Called internally by the SDK.
   */
  set(key: string, value: any): void {
    this._data[key] = value;
    this._dirty = true;
  }

  /**
   * Save visitor data from memory back to KV.
   * Call this AFTER SDK operations are complete.
   * @param visitorId - The visitor ID used in load()
   * @param ttl - KV entry TTL in seconds (default: 86400 = 24 hours)
   */
  async save(visitorId: string, ttl = 86400): Promise<void> {
    if (!this._dirty) return;
    await this._kv.put(
      `${this._prefix}:${visitorId}`,
      JSON.stringify(this._data),
      {expirationTtl: ttl}
    );
    this._dirty = false;
  }
}
