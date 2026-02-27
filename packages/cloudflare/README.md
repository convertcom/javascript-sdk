# @convertcom/js-sdk-cloudflare

Cloudflare Workers utilities for the [Convert JavaScript SDK](https://github.com/convertcom/javascript-sdk). Provides the glue code between the FullStack SDK and Cloudflare-specific APIs (HTMLRewriter, Workers Request/Response, edge caching).

## Installation

```bash
npm install @convertcom/js-sdk @convertcom/js-sdk-cloudflare
# or
yarn add @convertcom/js-sdk @convertcom/js-sdk-cloudflare
```

## What's Included

| Export | Purpose |
|--------|---------|
| `EdgeConfigCache` | Cache SDK config using Cloudflare's native `fetch` cache (no KV needed) |
| `KVDataStore` | **Optional** KV-backed DataStore adapter for persisting bucketing decisions |
| `getVisitorId` | Parse visitor ID from Workers Request cookies |
| `setVisitorIdCookie` | Set visitor ID cookie on Workers Response |
| `generateVisitorId` | Generate a new UUID visitor ID |
| `buildCacheKey` | Create variation-aware cache keys for Cloudflare's Cache API |
| `setCacheHeaders` | Set `Cache-Control` + `Vary: Cookie` headers |

## Quick Start

```typescript
import ConvertSDK from '@convertcom/js-sdk';
import {
  EdgeConfigCache,
  getVisitorId,
  setVisitorIdCookie,
  generateVisitorId
} from '@convertcom/js-sdk-cloudflare';

export default {
  async fetch(request, env, ctx) {
    // 1. Get config (cached at edge via Cloudflare's fetch cache — no KV)
    const config = await new EdgeConfigCache(env.SDK_KEY).getConfig();

    // 2. Init SDK
    const sdk = new ConvertSDK({ data: config, network: { tracking: true } });
    await sdk.onReady();

    // 3. Identify visitor
    const visitorId = getVisitorId(request) || generateVisitorId();

    // 4. Run experiment (deterministic bucketing — no persistence needed)
    const context = sdk.createContext(visitorId);
    const variation = context.runExperience('my-experiment');

    // 5. Modify the page with HTMLRewriter (zero flicker)
    const origin = await fetch(request);
    let response = origin;
    if (variation?.key === 'variation-1') {
      response = new HTMLRewriter()
        .on('h1', { element(el) { el.setInnerContent('New Headline'); } })
        .transform(origin);
    }

    // 6. Set cookie and respond
    const headers = new Headers(response.headers);
    setVisitorIdCookie(headers, visitorId);

    // 7. Flush tracking events before Worker finishes
    ctx.waitUntil(context.releaseQueues());

    return new Response(response.body, { status: response.status, headers });
  }
};
```

## Documentation

See the full guide: [Cloudflare Workers Edge Experimentation](https://github.com/convertcom/javascript-sdk/wiki/CloudflareWorkers)

## License

Apache-2.0
