import {readFileSync} from 'fs';
import {basename, resolve} from 'path';
import generateRollupConfig from './generate-rollup-config.mjs';

export default () => {
  const basePath = resolve('.');
  const packageName = basename(basePath);
  const input = {
    index: resolve('index.ts'),
    'goals-entry': resolve('src/standalone/goals-entry.ts'),
    'split-entry': resolve('src/standalone/split-entry.ts'),
    'visitor-entry': resolve('src/standalone/visitor-entry.ts'),
    'integrations-entry': resolve('src/standalone/integrations-entry.ts')
  };
  const info = JSON.parse(readFileSync(`${basePath}/package.json`, 'utf-8'));
  console.log(`build ${info.name}...`);
  return generateRollupConfig({basePath, input, info, packageName});
};
