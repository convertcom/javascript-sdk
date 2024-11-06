/* eslint-disable */
import {readFileSync, writeFileSync} from 'fs';
import {dirname, resolve} from 'path';
import {globSync} from 'glob';
import {babel} from '@rollup/plugin-babel';
import terser from '@rollup/plugin-terser';
import nodeResolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import typescript from 'rollup-plugin-typescript2';
import jsdoc from 'rollup-plugin-jsdoc';
import json from '@rollup/plugin-json';
import generatePackageJson from 'rollup-plugin-generate-package-json';
import copy from 'rollup-plugin-copy';
import modify from 'rollup-plugin-modify';
import dts from 'rollup-plugin-dts';

import dotenv from 'dotenv';
dotenv.config();

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

const DEFAULT_CONFIG_ENDPOINT = 'https://cdn-4.convertexperiments.com/api/v1/';
const DEFAULT_TRACK_ENDPOINT =
  'https://[project_id].metrics.convertexperiments.com/v1/';

const CONFIG_ENV = {
  find: 'process.env.CONFIG_ENDPOINT',
  replace: `'${process.env.CONFIG_ENDPOINT || DEFAULT_CONFIG_ENDPOINT}'`
};

const TRACK_ENV = {
  find: 'process.env.TRACK_ENDPOINT',
  replace: `'${process.env.TRACK_ENDPOINT || DEFAULT_TRACK_ENDPOINT}'`
};

const JSDOC_PATH = 'docs';

const exclude = [
  '**/*.conf.js',
  '**/*.tests.js',
  '**/build',
  '**/demo-*',
  `**/${JSDOC_PATH}`,
  '**/dist',
  '**/lib',
  '**/*.md',
  '**/rollup.config.js',
  '**/tests'
];

const depsMap = {
  logger: ['enums'],
  types: ['enums'],
  utils: ['enums'],
  event: ['logger', 'enums', 'types'],
  bucketing: ['logger', 'enums', 'types', 'utils'],
  rules: ['logger', 'enums', 'types', 'utils'],
  api: ['logger', 'enums', 'types', 'utils', 'event'],
  data: [
    'logger',
    'enums',
    'types',
    'utils',
    'event',
    'api',
    'bucketing',
    'rules'
  ],
  experience: ['logger', 'enums', 'types', 'data'],
  segments: ['logger', 'enums', 'types', 'data', 'rules'],
  'js-sdk': [
    'logger',
    'enums',
    'types',
    'utils',
    'api',
    'bucketing',
    'data',
    'event',
    'experience',
    'rules',
    'segments'
  ]
};

const external = ['querystring', 'http', 'https'];

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

const tsconfigOverride = (basePath, packageName, declaration = false) => ({
  compilerOptions: {
    declaration,
    baseUrl: resolve(basePath),
    paths: depsMap[packageName]
      ? Object.fromEntries(
          [
            depsMap[packageName].map((dep) => [
              `@convertcom/js-sdk-${dep}`,
              [resolve(basePath, '..', dep)]
            ]),
            depsMap[packageName].map((dep) => [
              `@convertcom/js-sdk-${dep}/*`,
              [resolve(basePath, '..', dep, 'src/*')]
            ])
          ].flat()
        )
      : {}
  },
  include: [resolve(basePath, '**/*')], // jail input files in package root
  exclude
});

const commonJSBundle = ({
  basePath,
  input,
  info,
  packageName,
  peerDependencies,
  isMainPackage
}) => ({
  cache: BUILD_CACHE,
  input,
  output: [
    {
      exports: 'named',
      file: resolve(basePath, 'lib', 'index.js'),
      format: 'cjs',
      sourcemap: true
    },
    {
      exports: 'named',
      file: resolve(basePath, 'lib', 'index.min.js'),
      format: 'cjs',
      sourcemap: true,
      plugins: [terser(terserConfig)]
    }
  ],
  plugins: withLogging.concat([
    modify(CONFIG_ENV),
    modify(TRACK_ENV),
    modify({
      find: 'process.env.VERSION',
      replace: `'js${info.version || 'js-sdk'}'`
    }),
    typescript({
      tsconfig: resolve(process.env.PROJECT_CWD, 'tsconfig.json'),
      tsconfigOverride: tsconfigOverride(basePath, packageName)
    }),
    nodeResolve(),
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
          directory: `packages/${packageName}`
        },
        license: 'Apache-2.0',
        ...(Object.keys(info.dependencies || {}).length
          ? {dependencies: info.dependencies}
          : {}),
        ...(peerDependencies ? {peerDependencies} : {}),
        version: pkg.version
      })
    }),
    ...(isMainPackage
      ? [
          jsdoc({
            args: ['-d', `${basePath}/${JSDOC_PATH}`],
            config: `${basePath}/jsdoc.config.json`
          }),
          copy({
            targets: [
              {
                src: [
                  `${basePath}/public/**/*`,
                  `${basePath}/coverage/coverage.svg`
                ],
                dest: `${basePath}/docs`
              }
            ]
          })
        ]
      : [])
  ])
});

const commonJSLegacyBundle = ({basePath, input, info, packageName}) => ({
  cache: BUILD_CACHE,
  input,
  output: [
    {
      exports: 'named',
      file: resolve(basePath, 'lib', 'legacy', 'index.js'),
      format: 'cjs',
      sourcemap: true
    },
    {
      exports: 'named',
      file: resolve(basePath, 'lib', 'legacy', 'index.min.js'),
      format: 'cjs',
      sourcemap: true,
      plugins: [terser(terserConfig)]
    }
  ],
  plugins: withLogging.concat([
    modify(CONFIG_ENV),
    modify(TRACK_ENV),
    modify({
      find: 'process.env.VERSION',
      replace: `'js${info.version || 'js-sdk'}'`
    }),
    typescript({
      tsconfig: resolve(process.env.PROJECT_CWD, 'tsconfig.json'),
      tsconfigOverride: tsconfigOverride(basePath, packageName)
    }),
    nodeResolve(),
    commonjs(),
    babel({
      babelHelpers: 'bundled',
      exclude: `${basePath}/node_modules/**`,
      presets: [['@babel/preset-env', {targets: 'defaults'}]]
    })
  ])
});

const esmBundle = ({basePath, input, info, packageName}) => ({
  cache: BUILD_CACHE,
  input,
  output: [
    {
      exports: 'auto',
      format: 'es',
      file: resolve(basePath, 'lib', 'index.mjs'),
      sourcemap: true
    },
    {
      exports: 'auto',
      format: 'es',
      file: resolve(basePath, 'lib', 'index.min.mjs'),
      plugins: [terser(terserConfig)],
      sourcemap: true
    }
  ],
  external,
  plugins: withLogging.concat([
    modify(CONFIG_ENV),
    modify(TRACK_ENV),
    modify({
      find: 'process.env.VERSION',
      replace: `'js${info.version || 'js-sdk'}'`
    }),
    typescript({
      tsconfig: resolve(process.env.PROJECT_CWD, 'tsconfig.json'),
      tsconfigOverride: tsconfigOverride(basePath, packageName, true)
    }),
    nodeResolve(),
    commonjs()
  ])
});

const umdBundle = ({basePath, input, info}) => ({
  cache: BUILD_CACHE,
  input,
  output: [
    {
      name: 'ConvertSDK',
      exports: 'named',
      format: 'umd',
      file: resolve(basePath, 'lib', 'index.umd.js'),
      sourcemap: true
    },
    {
      name: 'ConvertSDK',
      exports: 'named',
      format: 'umd',
      file: resolve(basePath, 'lib', 'index.umd.min.js'),
      plugins: [terser(terserConfig)],
      sourcemap: true
    }
  ],
  plugins: withLogging.concat([
    modify(CONFIG_ENV),
    modify(TRACK_ENV),
    modify({
      find: 'process.env.VERSION',
      replace: `'js${info.version || 'js-sdk'}'`
    }),
    typescript({
      tsconfig: resolve(process.env.PROJECT_CWD, 'tsconfig.json'),
      tsconfigOverride: tsconfigOverride(basePath)
    }),
    nodeResolve({
      mainFields: ['browser'],
      preferBuiltins: false
    }),
    commonjs(),
    json()
  ])
});

const replacePackages = (dir, file, packageName, contents) => {
  depsMap[packageName].forEach((dep) => {
    const relativePath = file.replace(`${dir}/`, '').split('/');
    const parents = new Array(relativePath.length).fill('').join('../');
    contents = contents.replaceAll(
      `@convertcom/js-sdk-${dep}`,
      `${parents || './'}${dep}`
    );
  });
  return contents;
};

const typeDeclarations = ({basePath, input, packageName, isMainPackage}) => ({
  cache: BUILD_CACHE,
  input,
  output: [
    {
      format: 'es',
      file: resolve(basePath, 'lib', 'index.d.ts')
    }
  ],
  external,
  plugins: [
    dts(),
    ...[
      depsMap[packageName]
        ? depsMap[packageName]
            .map((dep) =>
              copy({
                targets: [
                  {
                    src: [
                      `${process.env.PROJECT_CWD}/packages/${dep}/lib/src`,
                      `${process.env.PROJECT_CWD}/packages/${dep}/lib/index.d.ts`
                    ],
                    dest: `${basePath}/lib/${dep}`
                  }
                ]
              })
            )
            .concat([
              (() => ({
                name: 'js-sdk-types',
                writeBundle: {
                  sequential: true,
                  order: 'post',
                  async handler({file}) {
                    const dir = dirname(file);
                    const files = globSync(`${dir}/**/*.d.ts`);
                    files.push(file);
                    for (const i of files) {
                      const contents = readFileSync(i, 'utf-8');
                      writeFileSync(
                        i,
                        replacePackages(dir, i, packageName, contents)
                      );
                    }
                  }
                }
              }))()
            ])
        : []
    ]
  ]
});

const BUNDLES = process.env.BUNDLES
  ? process.env.BUNDLES.split(',')
  : ['cjs', 'cjs-legacy', 'esm', 'umd'];

export default async ({basePath, input, info, packageName}) => {
  const getVersion = (pkg) =>
    JSON.parse(
      readFileSync(
        resolve(
          `${basePath}/../${pkg.replace('@convertcom/js-sdk-', '')}/package.json`
        ),
        'utf-8'
      )
    );
  const peerDependencies = depsMap[packageName]
      ? Object.fromEntries(
          depsMap[packageName].map((dep) => [
            `@convertcom/js-sdk-${dep}`,
            `>=${getVersion(dep).version}`
          ])
        )
      : null,
    isMainPackage = packageName === 'js-sdk';
  if (peerDependencies) {
    console.log('peerDependencies:', peerDependencies);
    writeFileSync(
      resolve(`${basePath}/package.json`),
      JSON.stringify(
        {
          ...info,
          peerDependencies: {...info.peerDependencies, ...peerDependencies}
        },
        null,
        2
      )
    );
  }
  return BUNDLES.map((bundle) => {
    switch (bundle) {
      case 'cjs':
        return [
          commonJSBundle({
            basePath,
            input,
            info,
            packageName,
            peerDependencies,
            isMainPackage
          })
        ];
      case 'cjs-legacy':
        return [commonJSLegacyBundle({basePath, input, info, packageName})];
      case 'esm':
        return [
          esmBundle({basePath, input, info, packageName}),
          ...(!['enums', 'types', 'utils'].includes(packageName)
            ? [typeDeclarations({basePath, input, packageName, isMainPackage})]
            : [])
        ];
      case 'umd':
        return isMainPackage ? [umdBundle({basePath, input, info})] : [];
    }
    return [];
  }).flat();
};
