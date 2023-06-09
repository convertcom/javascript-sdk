import {babel} from '@rollup/plugin-babel';
import terser from '@rollup/plugin-terser';
import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import typescript from 'rollup-plugin-typescript2';
import jsdoc from 'rollup-plugin-jsdoc';
import json from '@rollup/plugin-json';
import generatePackageJson from 'rollup-plugin-generate-package-json';
import copy from 'rollup-plugin-copy';
import replace from '@rollup/plugin-replace';

import dotenv from 'dotenv';
dotenv.config();

const ENV_OPTIONS = {
  values: {
    'process.env.CONFIG_ENDPOINT': `'${process.env.CONFIG_ENDPOINT || ''}'`,
    'process.env.TRACK_ENDPOINT': `'${process.env.TRACK_ENDPOINT || ''}'`
  },
  objectGuards: true,
  preventAssignment: true
};

const JSDOC_PATH = 'docs';

const exclude = [
  '**/*.conf.js',
  '**/*.tests.js',
  '**/build',
  '**/demo',
  `**/${JSDOC_PATH}`,
  '**/dist',
  '**/lib',
  '**/*.md',
  '**/rollup.config.js',
  '**/tests'
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
  plugins: [
    replace(ENV_OPTIONS),
    typescript({
      tsconfigOverride: {exclude: exclude}
    }),
    resolve({
      browser: true,
      preferBuiltins: false
    }),
    commonjs(),
    json(),
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
          directory: 'packages/js-sdk'
        },
        license: 'Apache-2.0',
        dependencies: {},
        version: pkg.version
      })
    }),
    jsdoc({
      args: ['-d', JSDOC_PATH],
      config: 'jsdoc.config.json'
    }),
    copy({
      targets: [{src: ['public/**/*', 'coverage/coverage.svg'], dest: 'docs'}]
    })
  ]
};

const commonJSLegacyBundle = {
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
  plugins: [
    replace(ENV_OPTIONS),
    typescript({
      tsconfigOverride: {compilerOptions: {target: 'es5'}, exclude: exclude}
    }),
    resolve({
      browser: true,
      preferBuiltins: false
    }),
    commonjs(),
    babel({
      exclude: 'node_modules/**',
      presets: [['@babel/preset-env', {targets: 'defaults'}]]
    }),
    json()
  ]
};

const esmBundle = {
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
  plugins: [
    replace(ENV_OPTIONS),
    typescript({
      tsconfigOverride: {exclude: exclude}
    }),
    resolve({
      browser: true,
      preferBuiltins: false
    }),
    commonjs(),
    json()
  ]
};

const umdBundle = {
  input: './index.ts',
  output: [
    {
      name: 'ConvertSDK',
      exports: 'auto',
      format: 'umd',
      file: 'lib/index.umd.js',
      sourcemap: true
    },
    {
      name: 'ConvertSDK',
      exports: 'auto',
      format: 'umd',
      file: 'lib/index.umd.min.js',
      plugins: [terser(terserConfig)],
      sourcemap: true
    }
  ],
  plugins: [
    replace(ENV_OPTIONS),
    typescript({
      tsconfigOverride: {exclude: exclude}
    }),
    resolve({
      browser: true,
      preferBuiltins: false
    }),
    commonjs(),
    json()
  ]
};
export default () => {
  return [commonJSBundle, commonJSLegacyBundle, esmBundle, umdBundle];
};
