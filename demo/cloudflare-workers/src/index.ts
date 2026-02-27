/*!
 * Convert JS SDK - Cloudflare Workers Demo
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 *
 * This Worker demonstrates four edge experimentation patterns:
 *
 *   1. Page-level A/B test   - HTMLRewriter modifies page content at the edge
 *   2. Asset / image swap    - Replace images or stylesheets per variation
 *   3. Split URL redirect    - Serve entirely different origin pages
 *   4. SPA injection         - Inject bucketing decisions as JSON for client-side SPAs
 *
 * All patterns are flicker-free because modifications happen server-side
 * before the response reaches the browser.
 */

import ConvertSDK, {BucketedVariation} from '@convertcom/js-sdk';
import {
  EdgeConfigCache,
  getVisitorId,
  setVisitorIdCookie,
  generateVisitorId,
  buildCacheKey,
  setCacheHeaders
} from '@convertcom/js-sdk-cloudflare';

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

interface Env {
  CONVERT_SDK_KEY: string;
  // KV is optional — only needed if you enable the KVDataStore for
  // persisting visitor bucketing data across experience config changes.
  // CONVERT_KV: KVNamespace;
}

// ---------------------------------------------------------------------------
// SDK Singleton
// ---------------------------------------------------------------------------

// The SDK instance persists across requests within the same Worker isolate.
// Config is fetched from the Convert CDN and cached at the Cloudflare edge
// using the native `cf` fetch cache (no KV required).
// The initialization promise is cached to prevent race conditions when
// concurrent requests hit a cold Worker simultaneously.
let sdk: InstanceType<typeof ConvertSDK> | null = null;
let sdkReadyPromise: Promise<InstanceType<typeof ConvertSDK>> | null = null;

/**
 * Initialise (or reuse) the SDK singleton.
 * Uses EdgeConfigCache with Cloudflare's built-in fetch cache — the config
 * response is cached at the edge for the TTL duration, no KV needed.
 */
async function getSDK(env: Env): Promise<InstanceType<typeof ConvertSDK>> {
  if (sdk) return sdk;
  if (sdkReadyPromise) return sdkReadyPromise;

  sdkReadyPromise = (async () => {
    const configCache = new EdgeConfigCache(
      env.CONVERT_SDK_KEY,
      300 // cache TTL in seconds (5 minutes)
    );
    const configData = await configCache.getConfig();

    const newSdk = new ConvertSDK({
      data: configData,
      // Passing data directly avoids the timer-based refresh (setTimeout)
      // which is meaningless in a stateless Worker environment.
      network: {tracking: true}
    });
    await newSdk.onReady();
    sdk = newSdk;
    return sdk;
  })();

  return sdkReadyPromise;
}

// ---------------------------------------------------------------------------
// Worker Entry Point
// ---------------------------------------------------------------------------

export default {
  async fetch(
    request: Request,
    env: Env,
    ctx: ExecutionContext
  ): Promise<Response> {
    const url = new URL(request.url);

    // Only process HTML page requests (skip assets, API calls, etc.)
    const accept = request.headers.get('Accept') || '';
    if (
      !accept.includes('text/html') ||
      url.pathname.startsWith('/api/') ||
      url.pathname.match(/\.\w{2,4}$/)
    ) {
      return fetch(request);
    }

    try {
      // 1. Initialise the SDK
      const convert = await getSDK(env);

      // 2. Identify the visitor
      let visitorId = getVisitorId(request);
      const isNewVisitor = !visitorId;
      if (isNewVisitor) {
        visitorId = generateVisitorId();
      }

      // 3. Create visitor context
      //    No KV persistence needed — the SDK uses deterministic MurmurHash
      //    bucketing, so the same visitorId always gets the same variation.
      const context = convert.createContext(visitorId);
      if (!context) {
        return fetch(request);
      }

      // 4. Run experiments
      //    Replace the experience key with your actual experience key from Convert.
      const variation = context.runExperience('your-experience-key', {
        locationProperties: {url: url.pathname}
      });

      // If no valid variation (rule error, bucketing error, or null), passthrough
      if (!variation || typeof variation === 'string') {
        return fetch(request);
      }

      // 5. Fetch the origin page
      const originResponse = await fetch(request);

      // 6. Apply the variation using HTMLRewriter
      const modifiedResponse = applyVariation(originResponse, variation);

      // 7. Build response headers (visitor cookie + cache control)
      const headers = new Headers(modifiedResponse.headers);
      if (isNewVisitor) {
        setVisitorIdCookie(headers, visitorId);
      }
      setCacheHeaders(headers, 300);

      // 8. Release tracking events in the background
      //
      //    IMPORTANT: The SDK batches tracking events and releases them on a
      //    timer (setTimeout). In Cloudflare Workers, the isolate may finish
      //    before that timer fires, so events would be lost. You MUST call
      //    releaseQueues() explicitly to flush all pending tracking events
      //    before the Worker completes.
      //
      //    waitUntil() ensures the tracking POST completes even after the
      //    response is already sent to the visitor — no added latency.
      ctx.waitUntil(context.releaseQueues('edge-request-complete'));

      return new Response(modifiedResponse.body, {
        status: modifiedResponse.status,
        statusText: modifiedResponse.statusText,
        headers
      });
    } catch (error) {
      // On any SDK error, serve the origin page unmodified
      console.error('Convert SDK error:', error);
      return fetch(request);
    }
  }
} satisfies ExportedHandler<Env>;

// ---------------------------------------------------------------------------
// Pattern 1: Page-Level A/B Test (HTMLRewriter)
// ---------------------------------------------------------------------------

/**
 * Modify the HTML response based on the bucketed variation.
 *
 * HTMLRewriter is Cloudflare's streaming HTML parser. It modifies the response
 * as it streams through the Worker -- no buffering, no DOM parsing overhead.
 * The visitor receives the final page with zero flicker.
 */
function applyVariation(
  response: Response,
  variation: BucketedVariation
): Response {
  // Map variation keys to HTMLRewriter transformations.
  // Customize these selectors and content for your experiments.
  switch (variation.key) {
    case 'variation-1':
      return new HTMLRewriter()
        .on('h1.hero-title', {
          element(el) {
            el.setInnerContent('Welcome to the New Experience');
          }
        })
        .on('.cta-button', {
          element(el) {
            el.setInnerContent('Get Started Free');
            el.setAttribute('class', 'cta-button cta-button--primary');
          }
        })
        .transform(response);

    case 'variation-2':
      return new HTMLRewriter()
        .on('h1.hero-title', {
          element(el) {
            el.setInnerContent('Discover What Works Best');
          }
        })
        .on('img.hero-image', {
          element(el) {
            el.setAttribute('src', '/images/hero-v2.webp');
            el.setAttribute('alt', 'Updated hero image');
          }
        })
        .transform(response);

    default:
      // Control / original -- return unmodified
      return response;
  }
}

// ---------------------------------------------------------------------------
// Pattern 2: Asset / Image Swap
// ---------------------------------------------------------------------------

// To swap assets for an entire variation, use HTMLRewriter on specific selectors:
//
// function swapAssets(response: Response): Response {
//   return new HTMLRewriter()
//     .on('link[rel="stylesheet"][href*="main.css"]', {
//       element(el) {
//         el.setAttribute('href', '/css/main-v2.css');
//       }
//     })
//     .on('img[data-testable]', {
//       element(el) {
//         const src = el.getAttribute('src') || '';
//         el.setAttribute('src', src.replace('/images/', '/images/v2/'));
//       }
//     })
//     .transform(response);
// }

// ---------------------------------------------------------------------------
// Pattern 3: Split URL Redirect
// ---------------------------------------------------------------------------

// For split URL tests, serve a completely different origin page:
//
// if (variation.key === 'new-checkout') {
//   const newUrl = new URL(request.url);
//   newUrl.pathname = '/checkout-v2' + newUrl.pathname.replace('/checkout', '');
//   return fetch(new Request(newUrl.toString(), request));
// }

// ---------------------------------------------------------------------------
// Pattern 4: SPA Injection
// ---------------------------------------------------------------------------

// For SPAs, inject bucketing decisions as a global JS variable
// so the client-side app can apply them without a second round-trip:
//
// function injectDecisions(response: Response, variations: any[]): Response {
//   const decisions = JSON.stringify(
//     variations.filter((v) => v && typeof v !== 'string')
//   );
//   return new HTMLRewriter()
//     .on('head', {
//       element(el) {
//         el.append(
//           `<script>window.__CONVERT_DECISIONS__=${decisions};</script>`,
//           {html: true}
//         );
//       }
//     })
//     .transform(response);
// }

// ---------------------------------------------------------------------------
// Pattern 5: Edge-Cached Responses Per Variation
// ---------------------------------------------------------------------------

// Use Cloudflare's Cache API to cache origin responses per variation.
// This avoids hitting the origin for every request once a variation
// has been fetched at least once.
//
// async function fetchWithEdgeCache(
//   request: Request,
//   variationKey: string
// ): Promise<Response> {
//   const cache = caches.default;
//   const cacheKey = buildCacheKey(request, variationKey);
//
//   let response = await cache.match(cacheKey);
//   if (response) return response;
//
//   response = await fetch(request);
//   const cloned = new Response(response.body, response);
//   cloned.headers.set('Cache-Control', 'public, max-age=300');
//   ctx.waitUntil(cache.put(cacheKey, cloned.clone()));
//   return cloned;
// }

// ---------------------------------------------------------------------------
// Optional: KV-Backed Visitor Persistence
// ---------------------------------------------------------------------------

// If you need to persist bucketing decisions across experience config changes
// (e.g. ensuring a visitor stays in the same variation even when rules change),
// add KV support:
//
// 1. Add to Env: CONVERT_KV: KVNamespace;
// 2. Add to wrangler.toml: [[kv_namespaces]] binding/id
// 3. Use in the handler:
//
// import { KVDataStore } from '@convertcom/js-sdk-cloudflare';
//
// const dataStore = new KVDataStore(env.CONVERT_KV);
// await dataStore.load(visitorId);
// const context = convert.createContext(visitorId, { dataStore });
// // ... run experiments ...
// ctx.waitUntil(dataStore.save(visitorId));
