# Convert SDK — Cloudflare Workers Demo

A complete example of running Convert A/B tests at the Cloudflare edge with zero client-side flicker.

## What This Demonstrates

1. **Page-level A/B test** — HTMLRewriter modifies page content before delivery
2. **Asset / image swap** — Replace images or stylesheets per variation
3. **Split URL redirect** — Serve entirely different origin pages transparently
4. **SPA injection** — Inject bucketing decisions as JSON for client-side SPAs
5. **Edge caching** — Cache origin responses per variation

## Setup

### 1. Install Dependencies

```bash
yarn install
```

### 2. Configure

Edit `wrangler.toml`:
- Set your Convert SDK key (`CONVERT_SDK_KEY`) in the format `ACCOUNT_ID/PROJECT_ID`

### 3. Update Experiment Keys

In `src/index.ts`, replace `'your-experience-key'` with your actual experience key from the Convert dashboard.

### 4. Run

```bash
# Local development
yarn dev

# Deploy to production
yarn deploy

# View live logs
yarn tail
```

## How It Works

```
Visitor → Cloudflare Edge → Worker
                              ├── Fetch config (edge-cached via cf.cacheTtl, ~1-5ms)
                              ├── SDK: bucket visitor into variation (MurmurHash)
                              ├── Fetch origin page
                              ├── HTMLRewriter: modify HTML per variation
                              ├── Set visitor cookie
                              └── Respond (total edge overhead: ~5-8ms)

                              └── Background (waitUntil):
                                  └── Send tracking event to Convert
```

No KV namespace is required. Config is cached using Cloudflare's native `cf.cacheTtl` fetch option (free, all plans). Visitor bucketing is deterministic — the same visitor ID always gets the same variation via a cookie.

## Optional: KV-Backed Persistence

If you need to preserve bucketing across experience config changes or store custom visitor attributes, you can optionally add KV support. See the commented section at the bottom of `src/index.ts` for setup instructions.

## Documentation

Full guide: [Cloudflare Workers Edge Experimentation](https://github.com/convertcom/javascript-sdk/wiki/CloudflareWorkers)
