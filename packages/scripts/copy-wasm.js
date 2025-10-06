#!/usr/bin/env node
const fs = require('fs');
const path = require('path');

const root = path.resolve(__dirname, '..');
const libDir = path.join(root, 'js-sdk', 'lib');
const sdkRoot = path.resolve(root, '..', '..', 'decisions-core-sdk');
const candidates = [
  path.join(sdkRoot, 'pkg-node', 'browser', 'decisions_core_bg.wasm'),
  path.join(sdkRoot, 'pkg-node', 'decisions_core_bg.wasm'),
  path.join(sdkRoot, 'pkg-web', 'decisions_core_bg.wasm')
];

const destinations = [
  path.join(libDir, 'decisions_core_bg.wasm'),
  path.join(libDir, 'browser', 'decisions_core_bg.wasm')
];

const source = candidates.find((candidate) => fs.existsSync(candidate));

if (!source) {
  console.warn('[copy-wasm] Skipping copy; source wasm not found. Looked in:');
  candidates.forEach((candidate) => console.warn('  -', candidate));
  process.exit(0);
}

for (const dest of destinations) {
  fs.mkdirSync(path.dirname(dest), {recursive: true});
  fs.copyFileSync(source, dest);
}

console.log('[copy-wasm] Copied decisions_core_bg.wasm to lib/');
