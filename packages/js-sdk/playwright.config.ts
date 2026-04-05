import {defineConfig, devices} from '@playwright/test';

const PORT = process.env.PORT || 3939;

/**
 * Playwright configuration for JS SDK browser and integration tests.
 * See https://playwright.dev/docs/test-configuration.
 */
export default defineConfig({
  testDir: './tests',
  testMatch: ['browser/**/*.spec.ts', 'integration/**/*.spec.ts'],
  /* Run tests sequentially — SDK tests share state */
  fullyParallel: false,
  /* Fail the build on CI if you accidentally left test.only in the source code. */
  forbidOnly: !!process.env.CI,
  /* Retry on CI only */
  retries: process.env.CI ? 2 : 0,
  /* Single worker to avoid port conflicts and shared state issues */
  workers: 1,
  /* Reporter to use */
  reporter: [['list']],
  /* Shared settings for all the projects below */
  use: {
    /* Base URL to use in actions like `await page.goto('/')`. */
    baseURL: `http://localhost:${PORT}`,
    /* Collect trace when retrying the failed test */
    trace: 'retain-on-failure'
  },
  /* Each test is given 60 seconds */
  timeout: 60_000,

  /* Configure projects for major browsers */
  projects: [
    {
      name: 'chromium',
      use: {
        ...devices['Desktop Chrome'],
        launchOptions: {
          args: ['--disable-gpu', '--no-sandbox']
        }
      }
    }
  ],

  /* Run local test server before starting the tests */
  webServer: {
    command: `node tests/browser/test-server.js`,
    url: `http://localhost:${PORT}`,
    reuseExistingServer: false
  }
});
