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
  'experienceName', 'experienceType', 'id', 'is_baseline', 'key', 'name', 'status',
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

  test.describe('runVariation — web variation change rendering', () => {
    // Helper: build a synthetic BucketedVariation with the requested change list
    const makeFixture = () => ({
      variation: {
        id: '999',
        key: 'test-var',
        experienceId: 'rv-exp-1',
        experienceKey: 'rv-experience',
        experienceType: 'a/b',
        traffic_allocation: 10000,
        status: 'running',
        changes: [
          {
            id: 101,
            type: 'defaultCode',
            data: {
              css: '.rv-default-code { color: rgb(11, 22, 33); }',
              js: 'window.__rvDefaultCodeJS = true;'
            }
          },
          {
            id: 102,
            type: 'customCode',
            data: {
              css: '.rv-custom-code { color: rgb(44, 55, 66); }',
              custom_js: 'window.__rvCustomJS = true;'
            }
          },
          {
            id: 103,
            type: 'defaultRedirect',
            data: {
              original_pattern: 'foo',
              variation_pattern: 'bar'
            }
          },
          {
            id: 104,
            type: 'fullStackFeature',
            data: {feature_id: 42, variables_data: {x: 1}}
          }
        ]
      },
      experience: {
        id: 'rv-exp-1',
        key: 'rv-experience',
        type: 'a/b',
        global_css: '.rv-global { color: rgb(77, 88, 99); }',
        global_js: 'window.__rvGlobalJS = true;'
      }
    });

    // Reset DOM markers + window flags between tests
    const resetDomMarkers = async (page: Page) => {
      await page.evaluate(() => {
        const ids = [
          'conv-exp-rv-exp-1-global-css',
          'conv-exp-rv-exp-1-global-js',
          // Per-change markers are scoped by experience + variation + change id
          'conv-chg-rv-exp-1-999-101-css',
          'conv-chg-rv-exp-1-999-101-js',
          'conv-chg-rv-exp-1-999-102-css',
          'conv-chg-rv-exp-1-999-102-custom-js',
          'conv-chg-rv-exp-1-999-103-css',
          'conv-chg-rv-exp-1-999-103-js',
          'conv-chg-rv-exp-1-999-104-css'
        ];
        for (const id of ids) document.getElementById(id)?.remove();
        delete (window as any).__rvDefaultCodeJS;
        delete (window as any).__rvCustomJS;
        delete (window as any).__rvGlobalJS;
      });
    };

    test('Should inject experience global_css and global_js once', async ({
      page
    }) => {
      await setup(page);
      await resetDomMarkers(page);
      const result = await page.evaluate((fixture) => {
        const context = (window as any).__defaultContext();
        context.runVariation(fixture.variation, {
          experience: fixture.experience
        });
        return {
          globalCssText: document.getElementById('conv-exp-rv-exp-1-global-css')
            ?.textContent,
          globalJsRan: (window as any).__rvGlobalJS === true,
          globalJsExists: !!document.getElementById('conv-exp-rv-exp-1-global-js')
        };
      }, makeFixture());
      expect(result.globalCssText).toBe(
        '.rv-global { color: rgb(77, 88, 99); }'
      );
      expect(result.globalJsRan).toBe(true);
      expect(result.globalJsExists).toBe(true);
    });

    test('Should apply defaultCode change CSS and JS', async ({page}) => {
      await setup(page);
      await resetDomMarkers(page);
      const result = await page.evaluate((fixture) => {
        const context = (window as any).__defaultContext();
        context.runVariation(fixture.variation, {
          experience: fixture.experience
        });
        return {
          changeCssText: document.getElementById('conv-chg-rv-exp-1-999-101-css')?.textContent,
          changeJsRan: (window as any).__rvDefaultCodeJS === true
        };
      }, makeFixture());
      expect(result.changeCssText).toBe(
        '.rv-default-code { color: rgb(11, 22, 33); }'
      );
      expect(result.changeJsRan).toBe(true);
    });

    test('Should apply customCode change CSS and custom_js', async ({page}) => {
      await setup(page);
      await resetDomMarkers(page);
      const result = await page.evaluate((fixture) => {
        const context = (window as any).__defaultContext();
        context.runVariation(fixture.variation, {
          experience: fixture.experience
        });
        return {
          customCssText: document.getElementById('conv-chg-rv-exp-1-999-102-css')
            ?.textContent,
          customJsRan: (window as any).__rvCustomJS === true,
          customJsMarker: !!document.getElementById(
            'conv-chg-rv-exp-1-999-102-custom-js'
          )
        };
      }, makeFixture());
      expect(result.customCssText).toBe(
        '.rv-custom-code { color: rgb(44, 55, 66); }'
      );
      expect(result.customJsRan).toBe(true);
      expect(result.customJsMarker).toBe(true);
    });

    test('Should skip defaultRedirect change (no redirect attempted, no marker)', async ({
      page
    }) => {
      await setup(page);
      await resetDomMarkers(page);
      const startUrl = page.url();
      const result = await page.evaluate((fixture) => {
        const context = (window as any).__defaultContext();
        context.runVariation(fixture.variation, {
          experience: fixture.experience
        });
        return {
          redirectCssExists: !!document.getElementById(
            'conv-chg-rv-exp-1-999-103-css'
          ),
          redirectJsExists: !!document.getElementById(
            'conv-chg-rv-exp-1-999-103-js'
          )
        };
      }, makeFixture());
      expect(result.redirectCssExists).toBe(false);
      expect(result.redirectJsExists).toBe(false);
      // Page didn't navigate
      expect(page.url()).toBe(startUrl);
    });

    test('Should skip fullStackFeature change (no DOM injection)', async ({
      page
    }) => {
      await setup(page);
      await resetDomMarkers(page);
      const result = await page.evaluate((fixture) => {
        const context = (window as any).__defaultContext();
        context.runVariation(fixture.variation, {
          experience: fixture.experience
        });
        return {
          ffCssExists: !!document.getElementById('conv-chg-rv-exp-1-999-104-css'),
          ffJsExists: !!document.getElementById('conv-chg-rv-exp-1-999-104-js')
        };
      }, makeFixture());
      expect(result.ffCssExists).toBe(false);
      expect(result.ffJsExists).toBe(false);
    });

    test('Should be idempotent — calling twice does not duplicate styles/scripts', async ({
      page
    }) => {
      await setup(page);
      await resetDomMarkers(page);
      const result = await page.evaluate((fixture) => {
        const context = (window as any).__defaultContext();
        // First call
        context.runVariation(fixture.variation, {
          experience: fixture.experience
        });
        const firstGlobalJsRanAt = (window as any).__rvGlobalJS;
        // Reset the marker flag to detect a re-run
        (window as any).__rvGlobalJS = false;
        // Second call — should be a no-op (script element with same id already in DOM)
        context.runVariation(fixture.variation, {
          experience: fixture.experience
        });
        return {
          firstGlobalJsRan: firstGlobalJsRanAt === true,
          secondGlobalJsRan: (window as any).__rvGlobalJS === true,
          styleCount: document.querySelectorAll('#conv-exp-rv-exp-1-global-css')
            .length,
          scriptCount: document.querySelectorAll('#conv-exp-rv-exp-1-global-js')
            .length,
          changeCssCount: document.querySelectorAll(
            '#conv-chg-rv-exp-1-999-101-css'
          ).length,
          changeJsCount: document.querySelectorAll(
            '#conv-chg-rv-exp-1-999-101-js'
          ).length
        };
      }, makeFixture());
      expect(result.firstGlobalJsRan).toBe(true);
      expect(result.secondGlobalJsRan).toBe(false);
      expect(result.styleCount).toBe(1);
      expect(result.scriptCount).toBe(1);
      expect(result.changeCssCount).toBe(1);
      expect(result.changeJsCount).toBe(1);
    });

    test('Should no-op safely when bucketedVariation is null', async ({
      page
    }) => {
      await setup(page);
      await resetDomMarkers(page);
      const errored = await page.evaluate(() => {
        const context = (window as any).__defaultContext();
        try {
          context.runVariation(null as any);
          context.runVariation(undefined as any);
          return false;
        } catch {
          return true;
        }
      });
      expect(errored).toBe(false);
    });

    test('Should execute global_css → global_js → per-change css → js → custom_js in order', async ({
      page
    }) => {
      await setup(page);
      await resetDomMarkers(page);
      // The pipeline is documented in Context.runVariation's JSDoc.
      // Monkey-patch the appendChild on document.head BEFORE invoking
      // runVariation so we observe the synchronous insertion order — each
      // <style>/<script> append pushes its marker id into __execOrder
      // before runVariation continues to the next step. We also wrap the
      // CSS/JS strings so script execution itself contributes to the log
      // (proving JS actually ran, not just the script tag was inserted).
      const order = await page.evaluate(() => {
        (window as any).__execOrder = [];
        const head = document.head;
        const originalAppend = head.appendChild.bind(head);
        head.appendChild = function <T extends Node>(node: T): T {
          const el = node as unknown as HTMLElement;
          const id = el?.id || '';
          if (id.includes('global-css')) (window as any).__execOrder.push('exp-css');
          else if (id.includes('global-js'))
            (window as any).__execOrder.push('exp-js');
          else if (id.includes('-200-css'))
            (window as any).__execOrder.push('chg-css');
          else if (id.includes('-200-js'))
            (window as any).__execOrder.push('chg-js');
          else if (id.includes('-201-css'))
            (window as any).__execOrder.push('chg-custom-css');
          else if (id.includes('-201-custom-js'))
            (window as any).__execOrder.push('chg-custom-js');
          return originalAppend(node);
        } as typeof head.appendChild;

        const orderingFixture = {
          variation: {
            id: '999',
            key: 'ord-var',
            experienceId: 'rv-exp-1',
            experienceKey: 'rv-experience',
            experienceType: 'a/b',
            changes: [
              {
                id: 200,
                type: 'defaultCode',
                data: {css: '.ord-default {}', js: ';'}
              },
              {
                id: 201,
                type: 'customCode',
                data: {css: '.ord-custom {}', custom_js: ';'}
              }
            ]
          },
          experience: {
            id: 'rv-exp-1',
            key: 'rv-experience',
            type: 'a/b',
            global_css: '.ord-global {}',
            global_js: ';'
          }
        };

        const context = (window as any).__defaultContext();
        context.runVariation(orderingFixture.variation, {
          experience: orderingFixture.experience
        });
        head.appendChild = originalAppend;
        return (window as any).__execOrder as string[];
      });

      expect(order).toEqual([
        'exp-css',
        'exp-js',
        'chg-css',
        'chg-js',
        'chg-custom-css',
        'chg-custom-js'
      ]);
    });

    test('Should warn and continue when options.experience is missing and experienceKey is not in config', async ({
      page
    }) => {
      await setup(page);
      await resetDomMarkers(page);
      // No `experience` option passed AND the bucketedVariation's
      // experienceKey is not present in the SDK's config — runVariation
      // should log a warn and continue (no throw, no DOM injection).
      const result = await page.evaluate(() => {
        const context = (window as any).__defaultContext();
        // Capture warns via a logger spy (defaultContext uses console under
        // the hood for `warn`).
        const warns: any[] = [];
        const origWarn = console.warn;
        console.warn = (...args: any[]) => {
          warns.push(args);
          origWarn.apply(console, args);
        };
        let errored = false;
        try {
          context.runVariation({
            id: 'orphan-var',
            key: 'orphan',
            experienceId: 'does-not-exist',
            experienceKey: 'does-not-exist',
            changes: [
              {
                id: 999,
                type: 'defaultCode',
                data: {css: '.orphan { color: red; }'}
              }
            ]
          });
        } catch {
          errored = true;
        }
        console.warn = origWarn;
        return {
          errored,
          // The change CSS should still be applied — the warn is about the
          // missing experience config, not a hard error.
          cssApplied: !!document.getElementById('conv-chg-does-not-exist-orphan-var-999-css')
        };
      });
      expect(result.errored).toBe(false);
      expect(result.cssApplied).toBe(true);
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
