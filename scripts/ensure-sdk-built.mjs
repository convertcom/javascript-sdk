/**
 * The demos consume `@convertcom/js-sdk` through its published entry points
 * (`main` is ./lib/index.js), which only exist after a workspace build. Starting
 * a demo without one fails with a bare
 *   Cannot find package '.../@convertcom/js-sdk/lib/index.js'
 * which says nothing about the actual cause. Build once, then get out of the way.
 */
import {execSync} from 'node:child_process';
import {existsSync} from 'node:fs';
import {dirname, resolve} from 'node:path';
import {fileURLToPath} from 'node:url';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');

// One built entry point per workspace the demos import directly.
const builtEntries = [
  'packages/js-sdk/lib/index.js',
  'packages/enums/lib/index.js',
  'packages/types/lib/index.js'
];

const missing = builtEntries.filter((p) => !existsSync(resolve(root, p)));

if (missing.length === 0) {
  process.exit(0);
}

console.log(
  `[demo] SDK not built yet (missing ${missing[0]}) — running "yarn build" once.`
);
execSync('yarn build', {cwd: root, stdio: 'inherit'});
