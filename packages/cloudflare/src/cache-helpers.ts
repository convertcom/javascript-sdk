/*!
 * Convert JS SDK - Cloudflare Workers Utilities
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * Build a variation-aware cache key for Cloudflare's Cache API.
 *
 * Without this, Cloudflare's edge cache would serve the same cached response
 * to all visitors regardless of their bucketed variation. By appending the
 * variation key as a query parameter, each variation gets its own cache entry.
 *
 * @param request - The original incoming request
 * @param variationKey - The bucketed variation key (e.g. 'variation-1')
 * @returns A new Request with the variation key appended as a query parameter
 *
 * @example
 * ```typescript
 * const cacheKey = buildCacheKey(request, variation.key);
 * const cache = caches.default;
 * let response = await cache.match(cacheKey);
 * if (!response) {
 *   response = await fetch(request);
 *   await cache.put(cacheKey, response.clone());
 * }
 * ```
 */
export function buildCacheKey(request: Request, variationKey: string): Request {
  const url = new URL(request.url);
  url.searchParams.set('_conv_v', variationKey);
  return new Request(url.toString(), {
    method: request.method,
    headers: request.headers
  });
}

/**
 * Set cache control headers appropriate for A/B tested responses.
 *
 * Adds `Vary: Cookie` so that Cloudflare's cache correctly separates
 * responses by visitor cookie. Sets a configurable max-age.
 *
 * @param headers - The response Headers object to modify
 * @param maxAge - Cache max-age in seconds (default: 300 = 5 minutes)
 */
export function setCacheHeaders(headers: Headers, maxAge = 300): void {
  headers.set('Cache-Control', `public, max-age=${maxAge}`);
  // Ensure Vary includes Cookie so cached responses respect visitor bucketing
  const existingVary = headers.get('Vary');
  if (existingVary) {
    if (!existingVary.toLowerCase().includes('cookie')) {
      headers.set('Vary', `${existingVary}, Cookie`);
    }
  } else {
    headers.set('Vary', 'Cookie');
  }
}
