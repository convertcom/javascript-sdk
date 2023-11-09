// import info from './package.json' assert {type: 'json'}; // ExperimentalWarning: Importing JSON modules is an experimental feature. This feature could change at any time
import {readFileSync} from 'fs';
import {dirname} from 'path';
import {fileURLToPath} from 'url';
import {babel} from '@rollup/plugin-babel';
import terser from '@rollup/plugin-terser';
import commonjs from '@rollup/plugin-commonjs';
import typescript from 'rollup-plugin-typescript2';
import generatePackageJson from 'rollup-plugin-generate-package-json';
import modify from 'rollup-plugin-modify';
import dts from 'rollup-plugin-dts';

import dotenv from 'dotenv';
dotenv.config();
const info = JSON.parse(
  readFileSync(
    `${dirname(fileURLToPath(import.meta.url))}/package.json`,
    'utf-8'
  )
);
console.log(`build ${info.name}...`);

const BUILD_CACHE = Boolean(process.env.NODE_ENV === 'production');

const LOGGER_OPTIONS = {
  replace: '// eslint-disable-line'
};
const logLevel = process.env.LOG_LEVEL ? Number(process.env.LOG_LEVEL) : 0;
switch (logLevel) {
  case 1:
    console.log('log level:', 'debug');
    LOGGER_OPTIONS.find =
      /this\._loggerManager(\?)?\.(?!(debug|info|warn|error)).*?;$/gms;
    break;
  case 2:
    console.log('log level:', 'info');
    LOGGER_OPTIONS.find =
      /this\._loggerManager(\?)?\.(?!(info|warn|error)).*?;$/gms;
    break;
  case 3:
    console.log('log level:', 'warn');
    LOGGER_OPTIONS.find = /this\._loggerManager(\?)?\.(?!(warn|error)).*?;$/gms;
    break;
  case 4:
    console.log('log level:', 'error');
    LOGGER_OPTIONS.find = /this\._loggerManager(\?)?\.(?!(error)).*?;$/gms;
    break;
  case 5:
    console.log('log level:', 'silent');
    LOGGER_OPTIONS.find = /this\._loggerManager(\?)?\..*?;$/gms;
    break;
}

const include = [`${dirname(fileURLToPath(import.meta.url))}/**/*`]; // jail input files in package root

const exclude = [
  '**/*.conf.js',
  '**/*.tests.js',
  '**/build',
  '**/dist',
  '**/lib',
  '**/*.md',
  '**/build.mjs',
  '**/tests'
];

const external = [
  '@convertcom/js-sdk-enums',
  '@convertcom/js-sdk-utils',
  'murmurhash'
];

const minimizedFilesHeader =
  '/*!\n' +
  ' * Convert JS SDK\n' +
  ' * Version 1.0.0\n' +
  ' * Copyright(c) 2020-2022 Convert Insights, Inc\n' +
  ' * License Apache-2.0\n' +
  ' */';

const terserConfig = {
  format: {
    preamble: minimizedFilesHeader,
    comments: false
  }
};

const withLogging = logLevel > 0 ? [modify(LOGGER_OPTIONS)] : [];

const commonJSBundle = {
  cache: BUILD_CACHE,
  input: './index.ts',
  output: [
    {
      exports: 'auto',
      file: './lib/index.js',
      format: 'cjs',
      sourcemap: true
    },
    {
      exports: 'auto',
      file: './lib/index.min.js',
      format: 'cjs',
      sourcemap: true,
      plugins: [terser(terserConfig)]
    }
  ],
  external: external,
  plugins: withLogging.concat([
    typescript({
      tsconfigOverride: {
        compilerOptions: {declaration: false},
        include: include,
        exclude: exclude
      }
    }),
    commonjs(),
    generatePackageJson({
      baseContents: (pkg) => ({
        name: pkg.name,
        main: 'index.min.js',
        module: 'index.min.mjs',
        browser: 'index.umd.min.js',
        types: 'index.d.ts',
        files: ['**/**/*'],
        author: 'Convert Insights, Inc',
        repository: {
          type: 'git',
          url: 'git+https://github.com/convertcom/javascript-sdk.git',
          directory: 'packages/rules'
        },
        license: 'Apache-2.0',
        dependencies: {},
        version: pkg.version
      })
    })
  ])
};

const commonJSLegacyBundle = {
  cache: BUILD_CACHE,
  input: './index.ts',
  output: [
    {
      exports: 'auto',
      file: './lib/legacy/index.js',
      format: 'cjs',
      sourcemap: true
    },
    {
      exports: 'auto',
      file: './lib/legacy/index.min.js',
      format: 'cjs',
      sourcemap: true,
      plugins: [terser(terserConfig)]
    }
  ],
  external: external,
  plugins: withLogging.concat([
    typescript({
      tsconfigOverride: {
        compilerOptions: {target: 'es5', declaration: false},
        include: include,
        exclude: exclude
      }
    }),
    commonjs(),
    babel({
      babelHelpers: 'bundled',
      exclude: 'node_modules/**',
      presets: [['@babel/preset-env', {targets: 'defaults'}]]
    })
  ])
};

const esmBundle = {
  cache: BUILD_CACHE,
  input: './index.ts',
  output: [
    {
      exports: 'auto',
      format: 'es',
      file: 'lib/index.mjs',
      sourcemap: true
    },
    {
      exports: 'auto',
      format: 'es',
      file: 'lib/index.min.mjs',
      plugins: [terser(terserConfig)],
      sourcemap: true
    }
  ],
  external: external,
  plugins: withLogging.concat([
    typescript({
      tsconfigOverride: {
        compilerOptions: {declaration: false},
        include: include,
        exclude: exclude
      }
    }),
    commonjs()
  ])
};

const typeDeclarations = {
  cache: BUILD_CACHE,
  input: './index.ts',
  output: [
    {
      format: 'es',
      file: 'lib/index.d.ts'
    }
  ],
  plugins: [dts()]
};

const BUNDLES = process.env.BUNDLES
  ? process.env.BUNDLES.split(',')
  : ['cjs', 'cjs-legacy', 'esm'];

export default () => {
  return BUNDLES.map((bundle) => {
    switch (bundle) {
      case 'cjs':
        return [commonJSBundle];
      case 'cjs-legacy':
        return [commonJSLegacyBundle];
      case 'esm':
        return [esmBundle, typeDeclarations];
    }
    return [];
  }).flat();
};
