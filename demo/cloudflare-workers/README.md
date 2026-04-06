# Convert SDK — Cloudflare Workers Demo

Run Convert A/B tests at the Cloudflare edge with zero client-side flicker. This demo works out of the box with the Convert staging project (`10035569/10034190`) — the same credentials used by all other SDK demos.

## Routes

| Route | Location | Experiments | Features |
|-------|----------|-------------|----------|
| `/` | — | None (home page) | — |
| `/events` | `events` | `test-experience-ab-fullstack-1` | — |
| `/statistics` | `statistics` | All matching | `feature-4` |
| `/pricing` | `pricing` | All matching | `feature-5` |

## How It Works

```
Visitor → Cloudflare Worker (localhost:8787)
              ├── Fetch config from Convert CDN (edge-cached, ~1-5ms)
              ├── Map URL path to location property (e.g. /events → "events")
              ├── SDK: createContext → runExperience/runExperiences
              ├── Fetch origin page (localhost:8888)
              ├── HTMLRewriter: inject experiment results into HTML
              ├── Set visitor cookie + cache headers
              └── Respond (total overhead: ~5-8ms)

              └── Background (waitUntil):
                  └── Flush tracking events to Convert
```

No KV namespace is required. Config is cached using Cloudflare's native `cf.cacheTtl` fetch option (free, all plans). Visitor bucketing is deterministic — the same visitor ID always gets the same variation via a cookie.

## Setup

### 1. Install Dependencies

From the monorepo root:

```bash
yarn install
```

If the cloudflare package hasn't been built yet:

```bash
yarn cloudflare:build
```

You don't need to publish `@convertcom/js-sdk-cloudflare` to npm — Yarn workspaces symlinks it to the local `packages/cloudflare/` directory, and Wrangler's bundler follows symlinks.

### 2. Start the Origin Server

The demo includes a simple origin server (`origin/`) that serves HTML pages for each route. The Worker proxies to this origin and modifies the response with experiment results.

```bash
# Terminal 1
cd demo/cloudflare-workers
yarn origin
```

This starts on http://localhost:8888 with pages for `/`, `/events`, `/statistics`, and `/pricing`.

### 3. Start the Worker

```bash
# Terminal 2
cd demo/cloudflare-workers
yarn dev
```

This starts on http://localhost:8787.

### 4. Open in Browser

Visit http://localhost:8787/ and navigate to the different routes:

- **Events** — Shows the bucketed variation for `test-experience-ab-fullstack-1`
- **Statistics** — Shows all matching variations + `feature-4` status
- **Pricing** — Shows all matching variations (both experiences) + `feature-5` status

## Verification

### Config Fetch

Confirm the staging project config is accessible:

```bash
curl -s "https://cdn-4.convertexperiments.com/api/v1/config/10035569/10034190" | head -c 200
```

Should return JSON starting with `{"account_id":"10035569"...`.

### Visitor Cookie

```bash
curl -v http://localhost:8787/events 2>&1 | grep -i set-cookie
```

Expected: `Set-Cookie: convert_visitor_id=<uuid>; Path=/; ...`

### Deterministic Bucketing

```bash
# Get a visitor ID
VISITOR_ID=$(curl -s -D- http://localhost:8787/events 2>&1 | grep -io 'convert_visitor_id=[^;]*' | cut -d= -f2)
echo "Visitor: $VISITOR_ID"

# Same cookie → same variation every time
curl -s -b "convert_visitor_id=$VISITOR_ID" http://localhost:8787/events
curl -s -b "convert_visitor_id=$VISITOR_ID" http://localhost:8787/events
```

Both responses should show identical experiment results.

### HTMLRewriter

Compare the origin page vs the Worker-modified page:

```bash
# Origin (unmodified)
curl -s http://localhost:8888/events | grep experiment-results

# Worker (modified with bucketing results)
curl -s http://localhost:8787/events | grep experiment-results
```

The Worker response should contain variation names/keys instead of the placeholder text.

### Cache Headers

```bash
curl -s -D- http://localhost:8787/events 2>&1 | grep -iE 'cache-control|vary'
```

Expected:

```
Cache-Control: public, max-age=300
Vary: Cookie
```

### Tracking Events

Watch the Worker logs while making requests:

```bash
# Terminal 3
cd demo/cloudflare-workers
yarn tail
```

After visiting a page, you should see the SDK POST tracking data to Convert. In the Convert dashboard, the visitor count for the experience should increment.

### Error Fallback

Temporarily set an invalid SDK key in `wrangler.toml`:

```toml
CONVERT_SDK_KEY = "invalid/key"
```

Restart `yarn dev` and visit http://localhost:8787/events — should return the unmodified origin page (graceful degradation via try/catch).

### Bundle Validation

```bash
cd demo/cloudflare-workers
npx wrangler deploy --dry-run --outdir /tmp/cf-bundle
```

Should succeed with bundle size ~296 KiB / ~58 KiB gzipped.

## Checklist

| Test | What to Verify |
|------|---------------|
| Config fetch | `curl` to CDN config endpoint returns JSON |
| Wrangler bundle | `--dry-run` succeeds |
| Worker starts | `yarn dev` runs without errors |
| Origin server | `yarn origin` serves pages on port 8888 |
| New visitor cookie | `Set-Cookie: convert_visitor_id=<uuid>` in response |
| Deterministic bucketing | Same cookie → same variation on repeated requests |
| HTMLRewriter | Experiment results injected into HTML |
| Cache headers | `Cache-Control: public, max-age=300` + `Vary: Cookie` |
| Tracking events | Visitor count increments in Convert dashboard |
| Error fallback | Invalid SDK key → unmodified origin page returned |
| No KV required | All above works without any KV namespace configured |

## Configuration

The demo is pre-configured in `wrangler.toml`:

```toml
CONVERT_SDK_KEY = "10035569/10034190"   # Staging project
ORIGIN_URL = "http://localhost:8888"     # Local origin server
```

To use your own project, update these values and adjust the experience/feature keys in `src/index.ts`.

## Iterating on Package Changes

If you modify `packages/cloudflare/src/` files:

```bash
# Rebuild the package
yarn cloudflare:build

# Restart the Worker (Ctrl+C, then yarn dev again)
```

## Production Deployment

**Important:** The default `ORIGIN_URL` is `http://localhost:8888` which only works for local development. Deploying with this value will fail because `localhost` is not reachable from Cloudflare's network (you'll see a Cloudflare "Error 1003: Direct IP access not allowed" page).

For production, update `ORIGIN_URL` in `wrangler.toml` to a publicly accessible origin:

```toml
ORIGIN_URL = "https://your-site.com"
```

Then deploy:

```bash
yarn deploy
```

## Optional: KV-Backed Persistence

If you need to preserve bucketing across experience config changes or store custom visitor attributes, you can optionally add KV support. See the commented section at the bottom of `src/index.ts` for setup instructions.

## Troubleshooting

**"Cannot find module @convertcom/js-sdk-cloudflare"**
→ Run `yarn install` at the monorepo root to ensure workspace symlinks exist, then `yarn cloudflare:build`.

**"Convert config fetch failed: 404"**
→ Verify your SDK key. Test directly: `curl https://cdn-4.convertexperiments.com/api/v1/config/ACCOUNT_ID/PROJECT_ID`

**Experience returns null / string (RuleError)**
→ The experience key doesn't match, or location/audience rules don't match. Check that the experience is **Active** (not Draft/Paused) in Convert.

**HTMLRewriter changes not visible**
→ Add `console.log('variation:', variation)` in the Worker and check Wrangler logs to confirm bucketing.

**Tracking events not appearing in dashboard**
→ Confirm `ctx.waitUntil(context.releaseQueues(...))` is called. Check Wrangler logs for outbound POST requests.

**Page hangs / infinite loop**
→ All `fetch(request)` calls must go through `fetchOrigin()` which rewrites the URL to `ORIGIN_URL`. Direct `fetch(request)` loops back to the Worker.

## Documentation

Full guide: [Cloudflare Workers Edge Experimentation](https://github.com/convertcom/javascript-sdk/wiki/CloudflareWorkers)
