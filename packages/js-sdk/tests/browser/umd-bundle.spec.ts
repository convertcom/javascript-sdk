import {test, expect, Page} from '@playwright/test';

// Helper: navigate to UMD test page and wait for config to load
async function setupPage(page: Page) {
  await page.goto('/umd.html');
  await page.waitForFunction(
    () => (window as any).__CONFIG_LOADED__ === true,
    {timeout: 5000}
  );
}

// Helper: inject SDK factory and DataStore onto window so page.evaluate()
// callbacks can call window.__createSdk() / window.__createContext() without
// repeating the boilerplate every time.
async function injectHelpers(page: Page) {
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

// Combined setup: navigate + inject helpers
async function setup(page: Page) {
  await setupPage(page);
  await injectHelpers(page);
}

// Qualifying attributes used across multiple tests
const LOCATION_PROPS = {
  locationProperties: {url: 'https://convert.com/'},
  visitorProperties: {varName3: 'something'}
};

// Expected object keys for SDK return types (pre-sorted for assertion)
const cmp = (a: string, b: string) => a.localeCompare(b);
const VARIATION_KEYS = [
  'bucketingAllocation', 'changes', 'experienceId', 'experienceKey',
  'experienceName', 'id', 'is_baseline', 'key', 'name', 'status',
  'traffic_allocation'
].sort(cmp);
const FEATURE_KEYS = [
  'experienceId', 'experienceKey', 'experienceName', 'id', 'key',
  'name', 'status', 'variables'
].sort(cmp);
const FEATURE_DISABLED_KEYS = ['id', 'key', 'name', 'status'].sort(cmp);

test.describe('UMD bundle browser tests', () => {
  test.describe('Basic SDK instance', () => {
    test('Should have ConvertSDK as a global', async ({page}) => {
      await setupPage(page);
      const hasSDK = await page.evaluate(
        () => typeof (window as any).ConvertSDK !== 'undefined'
      );
      expect(hasSDK).toBe(true);
    });

    test('Should have a constructor', async ({page}) => {
      await setupPage(page);
      const isFunction = await page.evaluate(
        () => typeof (window as any).ConvertSDK.default === 'function'
      );
      expect(isFunction).toBe(true);
    });

    test('Should create a default SDK instance and fire ready event with no errors', async ({
      page
    }) => {
      await setup(page);
      const result = await page.evaluate(() => {
        return new Promise((resolve) => {
          const sdk = (window as any).__createSdk();
          sdk.on('ready', (_args: any, err: any) => {
            resolve({hasError: err !== null, isObject: typeof sdk === 'object'});
          });
        });
      });
      expect((result as any).hasError).toBe(false);
      expect((result as any).isObject).toBe(true);
    });

    test('Should create a default SDK instance and resolve onReady promise', async ({
      page
    }) => {
      await setup(page);
      const resolved = await page.evaluate(async () => {
        const sdk = (window as any).__createSdk();
        await sdk.onReady();
        return true;
      });
      expect(resolved).toBe(true);
    });

    test('Should successfully create visitor context', async ({page}) => {
      await setup(page);
      const result = await page.evaluate(() => {
        const context = (window as any).__defaultContext();
        const methods = [
          'runExperience',
          'runExperiences',
          'runFeature',
          'runFeatures',
          'trackConversion',
          'setDefaultSegments',
          'runCustomSegments'
        ];
        const hasMethods = methods.every(
          (m) => typeof context[m] === 'function'
        );
        return {isObject: typeof context === 'object', hasMethods};
      });
      expect((result as any).isObject).toBe(true);
      expect((result as any).hasMethods).toBe(true);
    });
  });

  test.describe('Basic SDK methods', () => {
    test('Should successfully get variation from specific experience', async ({
      page
    }) => {
      await setup(page);
      const result = await page.evaluate((props) => {
        const context = (window as any).__defaultContext();
        const variation = context.runExperience(
          'test-experience-ab-fullstack-2',
          props
        );
        return {
          isObject: typeof variation === 'object' && variation !== null,
          experienceKey: variation?.experienceKey,
          keys: variation ? Object.keys(variation).sort((a, b) => a.localeCompare(b)) : []
        };
      }, LOCATION_PROPS);
      expect((result as any).isObject).toBe(true);
      expect((result as any).experienceKey).toBe(
        'test-experience-ab-fullstack-2'
      );
      expect((result as any).keys).toEqual(VARIATION_KEYS);
    });

    test('Should successfully get variations across all experiences', async ({
      page
    }) => {
      await setup(page);
      const result = await page.evaluate((props) => {
        const context = (window as any).__defaultContext();
        const variations = context.runExperiences(props);
        return {
          isArray: Array.isArray(variations),
          length: variations?.length,
          ids: variations?.map((v: any) => v.id)
        };
      }, LOCATION_PROPS);
      expect((result as any).isArray).toBe(true);
      expect((result as any).length).toBe(2);
      const validIds = ['100299456', '100299457', '100299460', '100299461'];
      for (const id of (result as any).ids) {
        expect(validIds).toContain(id);
      }
    });

    test('Should successfully get a single feature and its status', async ({
      page
    }) => {
      await setup(page);
      const result = await page.evaluate((props) => {
        const context = (window as any).__defaultContext();
        const feature = context.runFeature('feature-2', props);
        return {
          isObject: typeof feature === 'object' && feature !== null,
          id: feature?.id,
          keys: feature ? Object.keys(feature).sort((a, b) => a.localeCompare(b)) : []
        };
      }, LOCATION_PROPS);
      expect((result as any).isObject).toBe(true);
      expect((result as any).id).toBe('10025');
      expect((result as any).keys).toEqual(FEATURE_KEYS);
    });

    test('Should successfully get multiple features and their status', async ({
      page
    }) => {
      await setup(page);
      const result = await page.evaluate((props) => {
        const context = (window as any).__defaultContext();
        const features = context.runFeature('feature-1', props);
        return {
          isArray: Array.isArray(features),
          length: features?.length,
          ids: features?.map((f: any) => f.id)
        };
      }, LOCATION_PROPS);
      expect((result as any).isArray).toBe(true);
      expect((result as any).length).toBe(2);
      const validIds = ['10024', '10025'];
      for (const id of (result as any).ids) {
        expect(validIds).toContain(id);
      }
    });

    test('Should successfully get features and their statuses', async ({
      page
    }) => {
      await setup(page);
      const result = await page.evaluate((props) => {
        const context = (window as any).__defaultContext();
        const features = context.runFeatures(props);
        return {
          isArray: Array.isArray(features),
          length: features?.length,
          ids: features?.map((f: any) => f.id),
          enabledKeys: features
            ?.filter((f: any) => f.status === 'enabled')
            .map((f: any) => Object.keys(f).sort((a, b) => a.localeCompare(b))),
          disabledKeys: features
            ?.filter((f: any) => f.status === 'disabled')
            .map((f: any) => Object.keys(f).sort((a, b) => a.localeCompare(b)))
        };
      }, LOCATION_PROPS);
      expect((result as any).isArray).toBe(true);
      expect((result as any).length).toBe(4);
      const validIds = ['10024', '10025', '10026'];
      for (const id of (result as any).ids.filter(
        (id: string) => id !== undefined
      )) {
        expect(validIds).toContain(id);
      }
      for (const keys of (result as any).enabledKeys) {
        expect(keys).toEqual(FEATURE_KEYS);
      }
      for (const keys of (result as any).disabledKeys) {
        expect(keys).toEqual(FEATURE_DISABLED_KEYS);
      }
    });

    test('Should trigger Conversion', async ({page}) => {
      await setup(page);
      const result = await page.evaluate((props) => {
        const context = (window as any).__defaultContext();
        context.runExperience('test-experience-ab-fullstack-2', props);
        const response = context.trackConversion('increase-engagement', {
          ruleData: {action: 'buy'},
          conversionData: [
            {key: 'amount', value: 10.3},
            {key: 'productsCount', value: 2}
          ]
        });
        return {isUndefined: response === undefined};
      }, LOCATION_PROPS);
      expect((result as any).isUndefined).toBe(true);
    });

    test('Should successfully set default segments', async ({page}) => {
      await setup(page);
      const result = await page.evaluate(() => {
        return new Promise((resolve) => {
          const {context, dataStore, storeKey} = (
            window as any
          ).__createSegmentTestContext();
          context.setDefaultSegments({country: 'US'});

          setTimeout(() => {
            const localSegments = dataStore.get(storeKey);
            resolve({
              hasSegments: localSegments?.segments !== undefined,
              segments: localSegments?.segments
            });
          }, 1100);
        });
      });
      expect((result as any).hasSegments).toBe(true);
      expect((result as any).segments).toEqual({
        country: 'US',
        browser: 'chrome'
      });
    });

    test('Should successfully set custom segments', async ({page}) => {
      await setup(page);
      const result = await page.evaluate(() => {
        return new Promise((resolve) => {
          const {context, dataStore, storeKey} = (
            window as any
          ).__createSegmentTestContext();
          context.runCustomSegments('test-segments-1', {
            ruleData: {enabled: true}
          });

          setTimeout(() => {
            const data = dataStore.get(storeKey);
            resolve({
              hasCustomSegments:
                data?.segments?.customSegments !== undefined,
              customSegments: data?.segments?.customSegments
            });
          }, 1100);
        });
      });
      expect((result as any).hasCustomSegments).toBe(true);
      expect((result as any).customSegments).toEqual(['200299434']);
    });
  });

  test.describe('Test invalid visitor', () => {
    // All SDK methods should return undefined when no visitor ID is set
    const invalidVisitorCases = [
      {method: 'runExperience', args: ['test-experience-ab-fullstack-2']},
      {method: 'runExperiences', args: []},
      {method: 'runFeature', args: ['feature-1']},
      {method: 'runFeatures', args: []},
      {method: 'trackConversion', args: ['increase-engagement']},
      {method: 'runCustomSegments', args: ['test-segments-1']}
    ];

    for (const {method, args} of invalidVisitorCases) {
      test(`Should fail ${method} if no visitor is set`, async ({page}) => {
        await setup(page);
        const result = await page.evaluate(
          ({m, a}) => {
            const context = (window as any).__createContext();
            return context[m](...a);
          },
          {m: method, a: args}
        );
        expect(result).toBeUndefined();
      });
    }
  });
});
