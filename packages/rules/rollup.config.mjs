import {babel} from '@rollup/plugin-babel';
import terser from '@rollup/plugin-terser';
import commonjs from '@rollup/plugin-commonjs';
import typescript from 'rollup-plugin-typescript2';
import generatePackageJson from 'rollup-plugin-generate-package-json';

const exclude = [
  '**/*.conf.js',
  '**/*.tests.js',
  '**/build',
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
  cache: false,
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
    typescript({
      tsconfigOverride: {exclude: exclude}
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
  ]
};

const commonJSLegacyBundle = {
  cache: false,
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
    typescript({
      tsconfigOverride: {compilerOptions: {target: 'es5'}, exclude: exclude}
    }),
    commonjs(),
    babel({
      exclude: 'node_modules/**',
      presets: [['@babel/preset-env', {targets: 'defaults'}]]
    })
  ]
};

const esmBundle = {
  cache: false,
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
    typescript({
      tsconfigOverride: {exclude: exclude}
    }),
    commonjs()
  ]
};

export default () => {
  return [commonJSBundle, commonJSLegacyBundle, esmBundle];
};
