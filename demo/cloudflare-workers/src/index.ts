/*!
 * Convert JS SDK - Cloudflare Workers Demo
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 *
 * This Worker demonstrates edge experimentation using the Convert FullStack SDK.
 * It proxies requests to an origin server and modifies HTML responses based on
 * A/B test bucketing decisions — all at the Cloudflare edge with zero flicker.
 *
 * Routes (matching the staging project's location rules):
 *   /          - Home page (no experiments)
 *   /events    - Runs experience "test-experience-ab-fullstack-1"
 *   /statistics - Runs all matching experiences + feature "feature-4"
 *   /pricing   - Runs all matching experiences + feature "feature-5"
 */

import ConvertSDK, {BucketedVariation} from '@convertcom/js-sdk';
import {
  EdgeConfigCache,
  getVisitorId,
  setVisitorIdCookie,
  generateVisitorId,
  setCacheHeaders
} from '@convertcom/js-sdk-cloudflare';

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

interface Env {
  CONVERT_SDK_KEY: string;
  // Origin server to proxy to. Set in wrangler.toml [vars].
  ORIGIN_URL: string;
  // KV is optional — only needed if you enable the KVDataStore for
  // persisting visitor bucketing data across experience config changes.
  // CONVERT_KV: KVNamespace;
}

// ---------------------------------------------------------------------------
// Route → Location mapping
// ---------------------------------------------------------------------------
// The staging project's locations match on a "location" property, not URL path.
// This mirrors the pattern used by the Node.js demo.

const ROUTE_LOCATION_MAP: Record<string, string> = {
  '/events': 'events',
  '/statistics': 'statistics',
  '/pricing': 'pricing'
};

// Experience and feature keys from the staging project [ConvertSDK]
const EXPERIENCE_KEY = 'test-experience-ab-fullstack-1';
const FEATURE_KEY_STATISTICS = 'feature-4'; // [ConvertSDK]
const FEATURE_KEY_PRICING = 'feature-5'; // [ConvertSDK]

// ---------------------------------------------------------------------------
// Origin fetch helper
// ---------------------------------------------------------------------------

/**
 * Fetch from the origin server instead of the Worker's own URL.
 * Without this, `fetch(request)` in local dev loops back to the Worker.
 */
function fetchOrigin(request: Request, env: Env): Promise<Response> {
  const originUrl = new URL(request.url);
  const origin = new URL(env.ORIGIN_URL);
  originUrl.hostname = origin.hostname;
  originUrl.port = origin.port;
  originUrl.protocol = origin.protocol;
  return fetch(new Request(originUrl.toString(), request));
}

// ---------------------------------------------------------------------------
// SDK Singleton
// ---------------------------------------------------------------------------

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
    const wantsHtml =
      accept.includes('text/html') || accept.includes('*/*') || accept === '';
    if (
      !wantsHtml ||
      url.pathname.startsWith('/api/') ||
      url.pathname.match(/\.\w{2,4}$/)
    ) {
      return fetchOrigin(request, env);
    }

    // Check if this route has a location mapping for experiments
    const location = ROUTE_LOCATION_MAP[url.pathname];
    if (!location) {
      // Home page or unknown route — serve origin unmodified with visitor cookie
      const response = await fetchOrigin(request, env);
      const headers = new Headers(response.headers);
      if (!getVisitorId(request)) {
        setVisitorIdCookie(headers, generateVisitorId());
      }
      return new Response(response.body, {
        status: response.status,
        statusText: response.statusText,
        headers
      });
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

      // 3. Create visitor context with audience properties
      //    The staging project's "Adv Audience" requires mobile: true or desktop: true.
      //    This mirrors the Node.js demo's createContext(userId, {mobile: true}).
      const context = convert.createContext(visitorId, {mobile: true});
      if (!context) {
        return fetchOrigin(request, env);
      }

      // Set default segments (mirrors Node.js demo)
      context.setDefaultSegments({country: 'US'});

      // 4. Run experiments based on route
      const locationProperties = {location};
      const decisions = decideForRoute(context, url.pathname, locationProperties);

      // 5. Fetch the origin page
      const originResponse = await fetchOrigin(request, env);

      // 6. Apply variations via HTMLRewriter
      const modifiedResponse = applyDecisions(originResponse, decisions);

      // 7. Build response headers (visitor cookie + cache control)
      const headers = new Headers(modifiedResponse.headers);
      if (isNewVisitor) {
        setVisitorIdCookie(headers, visitorId);
      }
      setCacheHeaders(headers, 300);

      // 8. Release tracking events in the background
      //    The SDK batches events with setTimeout which won't fire in Workers.
      //    releaseQueues() flushes immediately; waitUntil() keeps the isolate
      //    alive for the tracking POST without blocking the response.
      ctx.waitUntil(context.releaseQueues('edge-request-complete'));

      return new Response(modifiedResponse.body, {
        status: modifiedResponse.status,
        statusText: modifiedResponse.statusText,
        headers
      });
    } catch (error) {
      // On any SDK error, serve the origin page unmodified
      console.error('Convert SDK error:', error);
      return fetchOrigin(request, env);
    }
  }
} satisfies ExportedHandler<Env>;

// ---------------------------------------------------------------------------
// Route-specific experiment logic
// ---------------------------------------------------------------------------

interface RouteDecisions {
  variation: BucketedVariation | null;
  variations: BucketedVariation[];
  feature: any;
  route: string;
}

/**
 * Run experiments and features for the current route.
 * Mirrors the Node.js demo's per-route decide() functions.
 */
function decideForRoute(
  context: any,
  pathname: string,
  locationProperties: {location: string}
): RouteDecisions {
  const decisions: RouteDecisions = {
    variation: null,
    variations: [],
    feature: null,
    route: pathname
  };

  switch (pathname) {
    case '/events': {
      // Run a single experience (like Node.js events route)
      const bucketed = context.runExperience(EXPERIENCE_KEY, {
        locationProperties
      });
      console.log('bucketed variation:', bucketed);
      if (bucketed && typeof bucketed !== 'string') {
        decisions.variation = bucketed;
      }
      break;
    }

    case '/statistics': {
      // Run all matching experiences + feature-4 (like Node.js statistics route)
      const bucketedAll = context.runExperiences({locationProperties});
      console.log('bucketed variation(s):', bucketedAll);
      if (Array.isArray(bucketedAll)) {
        decisions.variations = bucketedAll.filter(
          (v: any) => v && typeof v !== 'string'
        );
      }
      const feature = context.runFeature(FEATURE_KEY_STATISTICS, {
        locationProperties
      });
      console.log('bucketed feature:', feature);
      if (feature && feature.status === 'enabled') {
        decisions.feature = feature;
      }
      break;
    }

    case '/pricing': {
      // Run all matching experiences + feature-5 (like Node.js pricing route)
      const bucketedAll = context.runExperiences({locationProperties});
      console.log('bucketed variation(s):', bucketedAll);
      if (Array.isArray(bucketedAll)) {
        decisions.variations = bucketedAll.filter(
          (v: any) => v && typeof v !== 'string'
        );
      }
      const feature = context.runFeature(FEATURE_KEY_PRICING, {
        locationProperties
      });
      console.log('bucketed feature:', feature);
      if (feature && feature.status === 'enabled') {
        decisions.feature = feature;
      }
      break;
    }
  }

  return decisions;
}

// ---------------------------------------------------------------------------
// HTMLRewriter transformations
// ---------------------------------------------------------------------------

/**
 * Apply experiment decisions to the origin HTML response.
 * Uses HTMLRewriter to inject bucketing results into the page.
 */
function applyDecisions(
  response: Response,
  decisions: RouteDecisions
): Response {
  const {variation, variations, feature, route} = decisions;

  // Build a summary of all decisions for this request
  const allVariations =
    variation ? [variation] : variations.length ? variations : [];
  if (allVariations.length === 0 && !feature) {
    return response; // No experiments matched — return unmodified
  }

  // Inject experiment results into the page
  return new HTMLRewriter()
    .on('#experiment-results', {
      element(el) {
        const items = allVariations
          .map(
            (v) =>
              `<li><strong>${v.experienceName || v.experienceKey}</strong>: ${v.name || v.key}</li>`
          )
          .join('');
        if (items) {
          el.setInnerContent(`<ul>${items}</ul>`, {html: true});
        }
      }
    })
    .on('#feature-status', {
      element(el) {
        if (feature) {
          el.setInnerContent(
            `<span class="enabled">Feature enabled: ${feature.key || 'yes'}</span>`,
            {html: true}
          );
        }
      }
    })
    .on('#variation-caption', {
      element(el) {
        // Extract the "caption" variable from feature-1 (attached to the experience)
        const v = allVariations[0];
        if (
          v &&
          Array.isArray(v.changes) &&
          v.changes.length &&
          v.changes[0].data?.variables_data?.caption
        ) {
          el.setInnerContent(v.changes[0].data.variables_data.caption);
        }
      }
    })
    .transform(response);
}

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
