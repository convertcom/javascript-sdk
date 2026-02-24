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

### 2. Create a KV Namespace

```bash
wrangler kv namespace create CONVERT_KV
```

Copy the output `id` into `wrangler.toml`.

### 3. Configure

Edit `wrangler.toml`:
- Set your KV namespace ID
- Set your Convert SDK key (`CONVERT_SDK_KEY`)

### 4. Update Experiment Keys

In `src/index.ts`, replace `'your-experience-key'` with your actual experience key from the Convert dashboard.

### 5. Run

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
                              ├── Read config from KV (cached, ~1ms)
                              ├── Read visitor data from KV (~1ms)
                              ├── SDK: bucket visitor into variation
                              ├── Fetch origin page
                              ├── HTMLRewriter: modify HTML per variation
                              ├── Set visitor cookie
                              └── Respond (total edge overhead: ~5-8ms)

                              └── Background (waitUntil):
                                  ├── Save bucketing to KV
                                  └── Send tracking event to Convert
```

## Documentation

Full guide: [Cloudflare Workers Edge Experimentation](https://github.com/convertcom/javascript-sdk/wiki/CloudflareWorkers)
