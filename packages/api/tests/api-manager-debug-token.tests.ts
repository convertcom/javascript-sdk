/* eslint-disable mocha/consistent-spacing-between-blocks */
import 'mocha';
import {expect} from 'chai';
import http from 'http';
import {ApiManager as am} from '../src/api-manager';
import {Config as ConfigType} from '@convertcom/js-sdk-types';
import {LogManagerInterface} from '@convertcom/js-sdk-logger';

// Dedicated host/port for this file so it never races with other
// api-manager test files' own server lifecycles.
const host = 'http://localhost';
const port = 8092;

const SDK_KEY = 'debug-token-sdk-key';
const DEBUG_TOKEN = 'test-debug-token-123';

/**
 * Builds a minimal, valid ApiManager config pointed at the local test
 * server, with `overrides` shallow-merged on top (network is merged one
 * level deep since it's the only nested key any case needs to touch).
 * Cast through `unknown` because `debugToken` does not exist on the
 * `ConfigBase` type yet (RED phase) — this must not be a compile error.
 */
function makeConfig(overrides: Record<string, any> = {}): ConfigType {
  const {network, ...rest} = overrides;
  return {
    sdkKey: SDK_KEY,
    data: {
      account_id: 'acct-1',
      project: {id: 'proj-1'}
    },
    api: {
      endpoint: {
        config: `${host}:${port}`,
        track: `${host}:${port}`
      }
    },
    ...(network ? {network} : {}),
    ...rest
  } as unknown as ConfigType;
}

type LoggedCall = {method: string; args: any[]};

/**
 * A fake LogManager recording every call made to it, so tests can assert
 * on what was (or wasn't) logged without depending on sinon.
 */
function makeSpyLogger(): LogManagerInterface & {calls: LoggedCall[]} {
  const calls: LoggedCall[] = [];
  const record = (method: string) => {
    return (...args: any[]) => {
      calls.push({method, args});
    };
  };
  return {
    log: record('log'),
    trace: record('trace'),
    debug: record('debug'),
    info: record('info'),
    warn: record('warn'),
    error: record('error'),
    addClient: record('addClient'),
    setClientLevel: record('setClientLevel'),
    calls
  } as LogManagerInterface & {calls: LoggedCall[]};
}

type ExpectedQuery = {
  hasDebugToken?: boolean;
  debugToken?: string;
  hasLowCache: boolean;
  hasEnvironment?: boolean;
  environment?: string;
};

/**
 * Single shared assertion for the AC1 case table: splits the captured
 * request URL into path + query, verifies the path is the expected
 * config route, and verifies the query string is well-formed (properly
 * `&`-joined, exactly the expected params, no more/less) using
 * URLSearchParams rather than substring checks.
 */
function assertQuery(requestUrl: string, expected: ExpectedQuery): void {
  const queryIndex = requestUrl.indexOf('?');
  const path = queryIndex === -1 ? requestUrl : requestUrl.slice(0, queryIndex);
  const queryString = queryIndex === -1 ? '' : requestUrl.slice(queryIndex + 1);

  expect(path).to.equal(`/config/${SDK_KEY}`);

  const expectedParamCount = [
    expected.hasDebugToken,
    expected.hasLowCache,
    expected.hasEnvironment
  ].filter(Boolean).length;

  if (expectedParamCount === 0) {
    expect(queryString).to.equal('');
    return;
  }

  const ampersandCount = (queryString.match(/&/g) || []).length;
  expect(ampersandCount).to.equal(
    expectedParamCount - 1,
    `expected ${expectedParamCount} params properly '&'-joined in "${queryString}"`
  );

  const params = new URLSearchParams(queryString);
  expect(Array.from(params.keys())).to.have.lengthOf(expectedParamCount);

  if (expected.hasDebugToken) {
    expect(params.get('debug_token')).to.equal(expected.debugToken);
  } else {
    expect(params.has('debug_token')).to.equal(false);
  }

  if (expected.hasLowCache) {
    expect(params.get('_conv_low_cache')).to.equal('1');
  } else {
    expect(params.has('_conv_low_cache')).to.equal(false);
  }

  if (expected.hasEnvironment) {
    expect(params.get('environment')).to.equal(expected.environment);
  } else {
    expect(params.has('environment')).to.equal(false);
  }
}

const debugTokenCases: Array<{
  name: string;
  overrides: Record<string, any>;
  expected: ExpectedQuery;
}> = [
  {
    name: 'debugToken set only',
    overrides: {debugToken: DEBUG_TOKEN},
    expected: {hasDebugToken: true, debugToken: DEBUG_TOKEN, hasLowCache: true}
  },
  {
    name: 'debugToken + network.cacheLevel "low"',
    overrides: {debugToken: DEBUG_TOKEN, network: {cacheLevel: 'low'}},
    expected: {hasDebugToken: true, debugToken: DEBUG_TOKEN, hasLowCache: true}
  },
  {
    name: 'network.cacheLevel "low" only (regression, no debugToken)',
    overrides: {network: {cacheLevel: 'low'}},
    expected: {hasDebugToken: false, hasLowCache: true}
  },
  {
    name: 'neither debugToken nor cacheLevel (regression)',
    overrides: {},
    expected: {hasDebugToken: false, hasLowCache: false}
  },
  {
    name: 'debugToken + environment',
    overrides: {debugToken: DEBUG_TOKEN, environment: 'staging'},
    expected: {
      hasDebugToken: true,
      debugToken: DEBUG_TOKEN,
      hasLowCache: true,
      hasEnvironment: true,
      environment: 'staging'
    }
  }
];

describe('ApiManager debugToken config option', function () {
  describe('AC1: config-fetch query string', function () {
    let server: http.Server;

    beforeEach(function () {
      server = http.createServer();
      server.listen(port);
    });

    afterEach(function () {
      server.closeAllConnections();
      server.close();
    });

    debugTokenCases.forEach(({name, overrides, expected}) => {
      it(`should build the /config request query correctly when ${name}`, function (done) {
        let finished = false;
        const finish = (err?: Error) => {
          if (finished) return;
          finished = true;
          done(err);
        };
        const apiManager = new am(makeConfig(overrides));
        server.on('request', (request, res) => {
          // Always answer the request first so the client never hangs
          // waiting on a response that an assertion failure would
          // otherwise prevent.
          res.writeHead(200, {'Content-Type': 'application/json'});
          res.end('{}');
          try {
            assertQuery(request.url, expected);
            finish();
          } catch (err) {
            finish(err as Error);
          }
        });
        apiManager.getConfig().catch((err) => finish(err));
      });
    });
  });

  describe('AC2: debug token hygiene', function () {
    let server: http.Server;
    let spyLogger: LogManagerInterface & {calls: LoggedCall[]};
    let apiManager;

    beforeEach(function () {
      server = http.createServer();
      server.listen(port);
      spyLogger = makeSpyLogger();
      apiManager = new am(makeConfig({debugToken: DEBUG_TOKEN}), {
        loggerManager: spyLogger
      });
    });

    afterEach(function () {
      server.closeAllConnections();
      server.close();
    });

    it('should never leak the debug token in the /track request URL or body, nor in logger output across the config, preview (?exp=), and track paths', function (done) {
      let finished = false;
      const finish = (err?: Error) => {
        if (finished) return;
        finished = true;
        done(err);
      };
      const requestData = {
        eventType: 'bucketing',
        data: {
          experienceId: 'exp-1',
          variationId: 'var-1'
        }
      };
      server.on('request', (request, res) => {
        if (!request.url.startsWith('/track')) {
          res.writeHead(200, {'Content-Type': 'application/json'});
          res.end('{}');
          return;
        }
        const body = [];
        request
          .on('data', (chunk) => {
            body.push(chunk);
          })
          .on('end', () => {
            // Always answer the request first so the client never hangs
            // waiting on a response that an assertion failure would
            // otherwise prevent.
            res.writeHead(200, {'Content-Type': 'application/json'});
            res.end('{}');
            try {
              expect(request.url).to.not.include(DEBUG_TOKEN);
              const rawBody = Buffer.concat(body).toString();
              expect(rawBody).to.not.include(DEBUG_TOKEN);
              const loggedText = JSON.stringify(spyLogger.calls);
              expect(loggedText).to.not.include(DEBUG_TOKEN);
              finish();
            } catch (err) {
              finish(err as Error);
            }
          });
      });
      apiManager
        .getConfig()
        // Also exercise the preview (?exp=) fetch path -- getConfigByExperience
        // appends debug_token to its own config URL too -- so the logger-hygiene
        // assertion below covers BOTH config-fetch seams, not just getConfig().
        .then(() => apiManager.getConfigByExperience('exp-preview-1'))
        .then(() => {
          apiManager.enqueue('visitor-1', requestData);
          apiManager.releaseQueue('test');
        })
        .catch((err) => finish(err));
    });
  });
});
