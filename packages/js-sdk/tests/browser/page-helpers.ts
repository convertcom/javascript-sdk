/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * Shared Playwright page bootstrap helpers for the js-sdk browser suite.
 *
 * Extracted from umd-bundle.spec.ts (originally defined inline there) so that
 * every browser spec file (umd-bundle.spec.ts, preview-zero-trace.spec.ts,
 * and any future addition) shares ONE copy of the UMD-page bootstrap instead
 * of re-declaring it per file -- this repo's SonarCloud quality gate fails a
 * PR once `new_duplicated_lines_density` on new/changed lines exceeds 3%,
 * and copy-pasting this ~35-line bootstrap block across spec files would
 * trip that gate immediately.
 */
import {Page} from '@playwright/test';

/** Navigate to the UMD test page and wait for the test config to load. */
export async function setupPage(page: Page): Promise<void> {
  await page.goto('/umd.html');
  await page.waitForFunction(
    () => (window as any).__CONFIG_LOADED__ === true,
    {timeout: 5000}
  );
}

/**
 * Inject SDK factory and DataStore helpers onto `window` so `page.evaluate()`
 * callbacks can call `window.__createSdk()` / `window.__createContext()`
 * without repeating the boilerplate every time.
 */
export async function injectHelpers(page: Page): Promise<void> {
  await page.evaluate(() => {
    const w = window as any;
    w.__createSdk = (extraConfig?: Record<string, any>) => {
      const config = {...w.__TEST_CONFIG__};
      config.events = {batch_size: 1, release_interval: 1000};
      Object.assign(config, extraConfig || {});
      return new w.ConvertSDK.default(config);
    };
    w.__createContext = (
      visitorId?: string,
      visitorProps?: Record<string, any>,
      extraConfig?: Record<string, any>
    ) => {
      const sdk = w.__createSdk(extraConfig);
      return sdk.createContext(visitorId, visitorProps);
    };
    // Shorthand: create context with default test visitor
    w.__defaultContext = (extraConfig?: Record<string, any>) =>
      w.__createContext('XXX', {browser: 'chrome'}, extraConfig);
    w.__makeDataStore = () => ({
      data: {} as Record<string, any>,
      get(key: string) {
        if (!key) return this.data;
        return this.data[key.toString()];
      },
      set(key: string, value: any) {
        if (!key) throw new Error('Invalid DataStore key!');
        this.data[key.toString()] = value;
      }
    });
    w.__createSegmentTestContext = () => {
      const dataStore = w.__makeDataStore();
      const sdk = w.__createSdk({dataStore});
      const accountId = w.__TEST_CONFIG__.data.account_id;
      const projectId = w.__TEST_CONFIG__.data.project.id;
      const visitorId = 'XXX';
      const storeKey = `${accountId}-${projectId}-${visitorId}`;
      const context = sdk.createContext(visitorId, {browser: 'chrome'});
      return {context, dataStore, storeKey};
    };
  });
}

/** Combined setup: navigate + inject helpers. */
export async function setup(page: Page): Promise<void> {
  await setupPage(page);
  await injectHelpers(page);
}
