import {readFileSync} from 'fs';
import path from 'path';
import generateRollupConfig from './generate-rollup-config.mjs';

export default () => {
  const basePath = path.resolve('.');
  const packageName = path.basename(basePath);
  const input = path.resolve('index.ts');
  const info = JSON.parse(readFileSync(`${basePath}/package.json`, 'utf-8'));
  console.log(`build ${info.name}...`);
  return generateRollupConfig({basePath, input, info, packageName});
};
