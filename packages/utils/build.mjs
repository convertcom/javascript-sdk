import {readFileSync} from 'fs';
import {dirname} from 'path';
import {fileURLToPath} from 'url';
import {babel} from '@rollup/plugin-babel';
import terser from '@rollup/plugin-terser';
import commonjs from '@rollup/plugin-commonjs';
import typescript from 'rollup-plugin-typescript2';
import generatePackageJson from 'rollup-plugin-generate-package-json';

const info = JSON.parse(
  readFileSync(
    `${dirname(fileURLToPath(import.meta.url))}/package.json`,
    'utf-8'
  )
);
console.log(`build ${info.name}...`);

const BUILD_CACHE = Boolean(process.env.NODE_ENV === 'production');

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
  'murmurhash',
  'querystring',
  'http',
  'https'
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
  plugins: [
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
          directory: 'packages/utils'
        },
        license: 'Apache-2.0',
        dependencies: {
          '@convertcom/js-sdk-enums': '>=1.0.0'
        },
        version: pkg.version
      })
    })
  ]
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
  plugins: [
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
  ]
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
  plugins: [
    typescript({
      tsconfigOverride: {
        compilerOptions: {declaration: true},
        include: include,
        exclude: exclude
      }
    }),
    commonjs()
  ]
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
        return [esmBundle];
    }
    return [];
  }).flat();
};
