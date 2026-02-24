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

import ConvertSDK from '@convertcom/js-sdk';
import {
  KVDataStore,
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
  CONVERT_KV: KVNamespace;
  CONVERT_SDK_KEY: string;
}

// ---------------------------------------------------------------------------
// SDK Singleton
// ---------------------------------------------------------------------------

// The SDK instance persists across requests within the same Worker isolate.
// Config is loaded from KV on the first request and reused afterwards.
let sdk: InstanceType<typeof ConvertSDK> | null = null;
let sdkReady = false;

/**
 * Initialise (or reuse) the SDK singleton.
 * Uses EdgeConfigCache so the config is served from KV (~1 ms) instead
 * of fetching from the CDN (~100 ms) on every cold start.
 */
async function getSDK(env: Env): Promise<InstanceType<typeof ConvertSDK>> {
  if (sdk && sdkReady) return sdk;

  const configCache = new EdgeConfigCache(
    env.CONVERT_KV,
    env.CONVERT_SDK_KEY,
    300 // cache TTL in seconds (5 minutes)
  );
  const configData = await configCache.getConfig();

  sdk = new ConvertSDK({
    data: configData,
    // Passing data directly avoids the timer-based refresh (setTimeout)
    // which is meaningless in a stateless Worker environment.
    network: {tracking: true}
  });
  await sdk.onReady();
  sdkReady = true;

  return sdk;
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

      // 3. Load persisted bucketing data from KV
      const dataStore = new KVDataStore(env.CONVERT_KV);
      await dataStore.load(visitorId);

      // 4. Create visitor context
      const context = convert.createContext(visitorId);
      if (!context) {
        return fetch(request);
      }

      // 5. Run experiments
      //    Replace the experience key with your actual experience key from Convert.
      const variation = context.runExperience('your-experience-key', {
        locationProperties: {url: url.pathname}
      });

      // If no valid variation (rule error, bucketing error, or null), passthrough
      if (!variation || typeof variation === 'string') {
        return fetch(request);
      }

      // 6. Fetch the origin page
      const originResponse = await fetch(request);

      // 7. Apply the variation using HTMLRewriter
      const modifiedResponse = applyVariation(originResponse, variation);

      // 8. Build response headers (visitor cookie + cache control)
      const headers = new Headers(modifiedResponse.headers);
      if (isNewVisitor) {
        setVisitorIdCookie(headers, visitorId);
      }
      setCacheHeaders(headers, 300);

      // 9. Persist bucketing data and release tracking events in the background
      //    waitUntil() ensures these complete even after the response is sent.
      ctx.waitUntil(
        Promise.all([
          dataStore.save(visitorId),
          context.releaseQueues('edge-request-complete')
        ])
      );

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
function applyVariation(response: Response, variation: any): Response {
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
