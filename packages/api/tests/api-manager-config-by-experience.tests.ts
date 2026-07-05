/* eslint-disable mocha/consistent-spacing-between-blocks */
import 'mocha';
import {expect} from 'chai';
import http from 'http';
import {ApiManager as am} from '../src/api-manager';
import {Config as ConfigType} from '@convertcom/js-sdk-types';

// Dedicated host/port for this file so it never races with the other
// api-manager test files' own server lifecycles (8090, 8092 already used).
const host = 'http://localhost';
const port = 8093;

const SDK_KEY = 'config-by-experience-sdk-key';
const DEBUG_TOKEN = 'test-debug-token-config-by-exp';

/**
 * Builds a minimal, valid ApiManager config pointed at the local test
 * server, with `overrides` shallow-merged on top of the defaults.
 */
function makeConfig(overrides: Record<string, any> = {}): ConfigType {
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
    ...overrides
  } as unknown as ConfigType;
}

/**
 * Instantiates an ApiManager pointed at the shared test server. Cast to
 * `any` because `getConfigByExperience` does not exist on the class or its
 * interface yet (RED phase) — the file must compile, the calls must fail
 * at runtime (method undefined).
 */
function makeApiManager(overrides: Record<string, any> = {}): any {
  return new am(makeConfig(overrides));
}

/**
 * Asserts that `requestUrl` targets the expected config route and carries
 * exactly `expectedParams` (name -> value), regardless of ordering, and
 * that the query string is `&`-joined into exactly that many segments.
 */
function assertConfigByExperienceQuery(
  requestUrl: string,
  expectedParams: Record<string, string>
): void {
  const [path, queryString = ''] = requestUrl.split('?');
  expect(path).to.equal(`/config/${SDK_KEY}`);

  const expectedKeys = Object.keys(expectedParams).sort();
  expect(queryString.split('&')).to.have.lengthOf(expectedKeys.length);

  const params = new URLSearchParams(queryString);
  expect(Array.from(params.keys()).sort()).to.deep.equal(expectedKeys);
  expectedKeys.forEach((key) => {
    expect(params.get(key)).to.equal(expectedParams[key]);
  });
}

describe('ApiManager getConfigByExperience', function () {
  describe('AC3: fetch half - request shape and resolved data', function () {
    let server: http.Server;

    beforeEach(function () {
      server = http.createServer();
      server.listen(port);
    });

    afterEach(function () {
      server.closeAllConnections();
      server.close();
    });

    const urlCases: Array<{
      name: string;
      overrides: Record<string, any>;
      buildExpectedParams: (expId: string) => Record<string, string>;
    }> = [
      {
        name: 'without debugToken configured',
        overrides: {},
        buildExpectedParams: (expId) => ({exp: expId, _conv_low_cache: '1'})
      },
      {
        name: 'with debugToken configured',
        overrides: {debugToken: DEBUG_TOKEN},
        buildExpectedParams: (expId) => ({
          exp: expId,
          _conv_low_cache: '1',
          debug_token: DEBUG_TOKEN
        })
      }
    ];

    urlCases.forEach(({name, overrides, buildExpectedParams}, index) => {
      it(`should build the /config request query correctly ${name}`, async function () {
        const expId = `exp-url-case-${index}`;
        const apiManager = makeApiManager(overrides);
        const responseBody = {exp: expId, marker: 'config-by-experience'};
        let capturedUrl = '';
        server.on('request', (request, res) => {
          capturedUrl = request.url;
          res.writeHead(200, {'Content-Type': 'application/json'});
          res.end(JSON.stringify(responseBody));
        });

        const result = await apiManager.getConfigByExperience(expId);

        assertConfigByExperienceQuery(capturedUrl, buildExpectedParams(expId));
        expect(result).to.deep.equal(responseBody);
      });
    });
  });

  describe('AC8: process-wide memoization with 60s TTL', function () {
    let server: http.Server;
    let hitCounts: Map<string, number>;
    const realDateNow = Date.now;

    beforeEach(function () {
      hitCounts = new Map();
      server = http.createServer();
      server.on('request', (request, res) => {
        const url = new URL(request.url, `${host}:${port}`);
        const exp = url.searchParams.get('exp');
        const count = (hitCounts.get(exp) || 0) + 1;
        hitCounts.set(exp, count);
        res.writeHead(200, {'Content-Type': 'application/json'});
        res.end(JSON.stringify({exp, hitCount: count}));
      });
      server.listen(port);
    });

    afterEach(function () {
      // Safety net: guarantee no patched Date.now leaks into any later
      // test/file even if a test body throws before reaching its own
      // restore.
      Date.now = realDateNow;
      server.closeAllConnections();
      server.close();
    });

    /**
     * Calls `getConfigByExperience(expId)` twice on `managerFirst` (or on
     * `managerSecond` for the 2nd call, when given), running
     * `betweenCalls` in between. Returns both resolved values so callers
     * can assert on equality/hit-counts without repeating the await
     * boilerplate.
     */
    async function callTwiceAndCollect(
      managerFirst: any,
      expId: string,
      options: {managerSecond?: any; betweenCalls?: () => void} = {}
    ): Promise<{first: any; second: any}> {
      const first = await managerFirst.getConfigByExperience(expId);
      if (options.betweenCalls) options.betweenCalls();
      const second = await (options.managerSecond ?? managerFirst)
        .getConfigByExperience(expId);
      return {first, second};
    }

    it('issues exactly one network fetch for two calls with the same id within the TTL window, both resolving to the same data', async function () {
      const apiManager = makeApiManager();
      const expId = 'exp-memo-same-id';

      const {first, second} = await callTwiceAndCollect(apiManager, expId);

      expect(hitCounts.get(expId)).to.equal(1);
      expect(second).to.deep.equal(first);
    });

    it('issues its own fetch for a different id', async function () {
      const apiManager = makeApiManager();
      const idA = 'exp-memo-diff-a';
      const idB = 'exp-memo-diff-b';

      await apiManager.getConfigByExperience(idA);
      await apiManager.getConfigByExperience(idB);

      expect(hitCounts.get(idA)).to.equal(1);
      expect(hitCounts.get(idB)).to.equal(1);
    });

    it('shares memoization across different ApiManager instances for the same sdkKey + experienceId (process-wide)', async function () {
      const expId = 'exp-memo-cross-instance';
      const apiManagerA = makeApiManager();
      const apiManagerB = makeApiManager();

      const {first, second} = await callTwiceAndCollect(apiManagerA, expId, {
        managerSecond: apiManagerB
      });

      expect(hitCounts.get(expId)).to.equal(1);
      expect(second).to.deep.equal(first);
    });

    it('does not share memoization across different sdkKeys for the same experienceId', async function () {
      const expId = 'exp-memo-cross-sdkkey';
      const apiManagerA = makeApiManager({sdkKey: `${SDK_KEY}-a`});
      const apiManagerB = makeApiManager({sdkKey: `${SDK_KEY}-b`});

      await callTwiceAndCollect(apiManagerA, expId, {
        managerSecond: apiManagerB
      });

      expect(hitCounts.get(expId)).to.equal(2);
    });

    it('refetches after the 60s TTL elapses for the same id', async function () {
      const apiManager = makeApiManager();
      const expId = 'exp-memo-ttl';

      const {first, second} = await callTwiceAndCollect(apiManager, expId, {
        betweenCalls: () => {
          const advanced = realDateNow() + 60001;
          Date.now = () => advanced;
        }
      });

      expect(hitCounts.get(expId)).to.equal(2);
      expect(second).to.not.deep.equal(first);
    });
  });
});
