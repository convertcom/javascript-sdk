/**
 * Everything a demo needs before it can start, so that `yarn demo:<x>:start`
 * works on a fresh clone with no manual setup.
 *
 * 1. Each demo reads its port from `.env`, which is gitignored and therefore
 *    absent after a clone. Without it the demos fall back to framework defaults
 *    and collide with each other on :3000 instead of the ports their READMEs
 *    document (reactjs 3002, nodejs 3003, nestjs 3004, nextjs 3005,
 *    remix client 3006, remix server 3007). Seed it from `.env.example`.
 *
 * 2. The demos consume `@convertcom/js-sdk` through its published entry points
 *    (`main` is ./lib/index.js), which only exist after a workspace build.
 *    Starting a demo without one fails with a bare
 *      Cannot find package '.../@convertcom/js-sdk/lib/index.js'
 *    which says nothing about the actual cause.
 *
 * Both steps are no-ops once satisfied, so this stays cheap on every start.
 */
import {execSync} from 'node:child_process';
import {copyFileSync, existsSync, readdirSync} from 'node:fs';
import {dirname, join, resolve} from 'node:path';
import {fileURLToPath} from 'node:url';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');

// --- 1. seed each demo's .env from its .env.example -------------------------
const demoDir = join(root, 'demo');
for (const name of readdirSync(demoDir)) {
  const example = join(demoDir, name, '.env.example');
  const env = join(demoDir, name, '.env');
  if (existsSync(example) && !existsSync(env)) {
    copyFileSync(example, env);
    console.log(`[demo] seeded demo/${name}/.env from .env.example`);
  }
}

// --- 2. build the workspace packages the demos import ----------------------
const builtEntries = [
  'packages/js-sdk/lib/index.js',
  'packages/enums/lib/index.js',
  'packages/types/lib/index.js',
  'packages/cloudflare/lib/index.js'
];

const missing = builtEntries.filter((p) => !existsSync(resolve(root, p)));
if (missing.length > 0) {
  console.log(
    `[demo] SDK not built yet (missing ${missing[0]}) — running "yarn build" once.`
  );
  execSync('yarn build', {cwd: root, stdio: 'inherit'});
}
