# Testing Guide

This document covers how to run and write tests for the `@convertcom/js-sdk` package.

## Prerequisites

- Node.js >= 22
- Yarn (via corepack: `corepack enable && corepack prepare yarn@stable --activate`)
- Playwright Chromium browser: `npx playwright install --with-deps chromium`
- Built SDK bundles (run `yarn build` before browser/integration tests)

## Test Suites

The SDK has three test suites:

| Suite | Runner | Scope | Command |
|-------|--------|-------|---------|
| **Unit tests** | Mocha + Chai | Core logic, context, feature manager, utilities | `yarn test:mocha` |
| **Browser tests** | Playwright | UMD bundle loaded in Chromium | `yarn test:browser` |
| **Integration tests** | Playwright | Full SDK lifecycle (init, bucket, feature, conversion) | `yarn test:browser` |

### Run everything

```bash
yarn build
yarn test:mocha
yarn test:browser
```

Or use the combined command (includes coverage):

```bash
yarn build
yarn test
```

## Unit Tests (Mocha)

Located in `tests/**/*.tests.ts`. These test SDK internals using Mocha + Chai with `ts-node/register`.

```bash
yarn test:mocha
```

Test files:
- `tests/core.tests.ts` — Core class (init, context creation, events)
- `tests/context.tests.ts` — Context class (runExperience, runFeature, trackConversion)
- `tests/feature-manager.tests.ts` — Feature manager logic
- `tests/utils/*.tests.ts` — Array, object, string, and comparison utilities

These tests use `tests/test-config.json` (fabricated IDs) and do **not** require network access.

## Browser Tests (Playwright)

Located in `tests/browser/`. These verify the built UMD bundle works correctly in a real browser environment.

```bash
yarn build          # Must build first — tests serve the built bundles
yarn test:browser
```

**How it works:**
1. Playwright starts a local HTTP server (`tests/browser/test-server.js`) on port 3939
2. The server serves the built UMD bundle (`lib/index.umd.min.js`) and a test HTML page
3. Tests navigate to the page and use `page.evaluate()` to exercise the SDK in browser context

Test file:
- `tests/browser/umd-bundle.spec.ts` — 19 tests covering SDK instantiation, experiences, features, conversions, segments, and invalid visitor handling

These tests use `tests/test-config.json` (fabricated IDs) and do **not** require network access.

## Integration Tests (Playwright)

Located in `tests/integration/`. These run the full SDK lifecycle in Node.js context (not browser), matching the PHP SDK's `FullChainIntegrationTest` pattern.

```bash
yarn build          # Must build first — tests import from lib/
yarn test:browser   # Integration tests run as part of the Playwright suite
```

Test file:
- `tests/integration/full-chain.spec.ts` — 17 tests per mode covering:
  - **Happy path:** init, ready event, experience bucketing, bucketing determinism, bucketing events, typed feature variables, full chain verification
  - **Negative path:** unknown feature key, non-qualifying location, audience mismatch
  - **Conversion tracking:** basic conversion, conversion events, goal deduplication, revenue tracking, forced multiple transactions, nonexistent goal
  - **Complete chain:** init -> context -> bucket -> feature -> conversion -> flush

### Auth Modes

Integration tests run in up to 3 modes, following the PHP SDK pattern:

| Mode | Config source | Env vars required | Always runs? |
|------|--------------|-------------------|-------------|
| `static` | `tests/integration/static-config.json` | None | Yes |
| `live` | CDN fetch (public key) | `CONVERT_STAGING_SDK_KEY` | No |
| `live-secret` | CDN fetch (authenticated) | `CONVERT_STAGING_SDK_KEY2`, `CONVERT_STAGING_SDK_KEY2_SECRET` | No |

The `static` mode always runs using a snapshot of the staging project config. The `live` and `live-secret` modes are skipped when the corresponding env vars are not set.

### Setting up env vars for live tests

Copy `.env.example` to `.env` and fill in the values:

```bash
# Public SDK key for unauthenticated CDN fetch
CONVERT_STAGING_SDK_KEY=<public-sdk-key>

# SDK key + secret for authenticated CDN fetch
CONVERT_STAGING_SDK_KEY2=<sdk-key>
CONVERT_STAGING_SDK_KEY2_SECRET=<sdk-key-secret>
```

Then run with the env vars loaded:

```bash
export $(grep -v '^#' .env | xargs)
yarn build
yarn test:browser
```

### Staging Project

All integration tests use the shared staging project **"FS-Test-Proj - DO NOT DELETE"** (account `10035569`, project `10034190`). This is the same project used by the PHP SDK's integration tests.

Key entities:
- **Experience:** `test-experience-ab-fullstack-4` — 50/50 split, pricing-location, no audiences
- **Feature-1:** boolean `enabled`, string `caption`
- **Feature-2:** float `price` (100), integer `button-height` (40), json `additionalData`
- **Goals:** `increase-engagement` (dom_interaction, no rules), `decrease-bounce-rate` (advanced)

**Do not modify or delete this project.** Changes will break integration tests in both the JS and PHP SDKs.

## Playwright Configuration

Config file: `playwright.config.ts`

- **Test server:** Auto-started on port 3939 (configurable via `PORT` env var)
- **Browser:** Chromium only, headless, with `--no-sandbox`
- **Workers:** 1 (sequential execution — tests share SDK state)
- **Timeout:** 60 seconds per test
- **Retries:** 2 on CI, 0 locally
- **Traces:** Retained on failure

## CI

Tests run in GitHub Actions via `.github/workflows/qa.yml`:

```
yarn → build → lint → test:mocha → test:browser
```

Live integration tests run in CI when the `CONVERT_STAGING_SDK_KEY`, `CONVERT_STAGING_SDK_KEY2`, and `CONVERT_STAGING_SDK_KEY2_SECRET` secrets are configured in the repository.

## Writing New Tests

### Adding a unit test

Add a `.tests.ts` file under `tests/`. It will be picked up automatically by the mocha glob `tests/**/*.tests.ts`.

```typescript
import 'mocha';
import {expect} from 'chai';

describe('MyFeature', () => {
  it('should do something', () => {
    expect(true).to.be.true;
  });
});
```

### Adding a browser test

Add assertions to `tests/browser/umd-bundle.spec.ts` or create a new `.spec.ts` file under `tests/browser/`.

```typescript
import {test, expect} from '@playwright/test';

test('SDK does something in browser', async ({page}) => {
  await page.goto('/');
  const result = await page.evaluate(() => {
    // ConvertSDK is available as a global from the UMD bundle
    return typeof ConvertSDK;
  });
  expect(result).toBe('function');
});
```

### Adding an integration test

Add tests inside the `for (const mode of modes)` loop in `tests/integration/full-chain.spec.ts` to ensure they run in all auth modes.

```typescript
test('My new integration test', async () => {
  const sdk = createSdk(mode);
  await sdk.onReady();
  const context = sdk.createContext('my-visitor-id');
  // ... exercise SDK and assert
});
```
