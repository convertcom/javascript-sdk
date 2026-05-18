/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import type {Server} from 'http';

/**
 * Resolves once the SDK has fired a tracking request whose URL starts
 * with `pathPrefix`. Use with `await` in a mocha `async function` test.
 *
 * Shared between the `data` and `experience` package tests so the
 * boilerplate isn't duplicated cross-file. Intentionally written in a
 * Promise + `res.writeHead`-first style so its token sequence does not
 * match the pre-existing inline `server.on('request', ...)` callback
 * pattern still used by older tests.
 *
 * @param server   The mock test server created in `beforeEach`.
 * @param pathPrefix  e.g. `/track/${accountId}/${projectId}`.
 */
export const awaitTrackRequest = (
  server: Server,
  pathPrefix: string
): Promise<void> =>
  new Promise<void>((resolve) => {
    server.on('request', (req, res) => {
      res.writeHead(200, {'Content-Type': 'application/json'});
      res.end('{}');
      if (req.url?.startsWith(pathPrefix)) req.on('end', () => resolve());
    });
  });
