import {readFileSync} from 'fs';
import {basename, resolve} from 'path';
import generateRollupConfig from './generate-rollup-config.mjs';

export default () => {
  const basePath = resolve('.');
  const packageName = basename(basePath);
  const input = resolve('index.ts');
  const info = JSON.parse(readFileSync(`${basePath}/package.json`, 'utf-8'));
  console.log(`build ${info.name}...`);
  return generateRollupConfig({basePath, input, info, packageName});
};
