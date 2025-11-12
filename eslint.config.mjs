import {FlatCompat} from '@eslint/eslintrc';
import js from '@eslint/js';
import {fileURLToPath} from 'url';
import {dirname} from 'path';
import mochaPlugin from 'eslint-plugin-mocha';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const compat = new FlatCompat({
  baseDirectory: __dirname,
  resolvePluginsRelativeTo: __dirname,
  recommendedConfig: js.configs.recommended
});

export default [
  {
    files: ['**/*.ts', '**/*.tsx'],
    ignores: ['.yarn', '.vscode', '.github', 'node_modules']
  },
  ...compat.extends(
    'eslint:recommended',
    'plugin:@typescript-eslint/eslint-recommended',
    'plugin:@typescript-eslint/recommended',
    // Prettier plugin and recommended rules
    'plugin:prettier/recommended'
  ),
  // Import mocha plugin directly (ES module)
  mochaPlugin.configs.recommended,
  ...compat.plugins('eslint-plugin-local-rules'),
  ...compat.config({
    root: true,
    parser: '@typescript-eslint/parser',
    rules: {
      // Include .prettierrc.js rules
      'prettier/prettier': ['error', {}, {usePrettierrc: true}],
      '@typescript-eslint/explicit-function-return-type': 'off',
      '@typescript-eslint/ban-ts-ignore': 'off',
      indent: ['error', 2, {SwitchCase: 1}],
      quotes: ['error', 'single'],
      semi: ['error', 'always'],
      'no-console': 'off',
      'linebreak-style': ['error', 'unix'],
      '@typescript-eslint/no-explicit-any': 'off',
      '@typescript-eslint/no-unused-vars': 'off',
      '@typescript-eslint/no-var-requires': 'off',
      'local-rules/log-manager': 'error'
    },
    env: {
      browser: true,
      node: true,
      commonjs: true,
      mocha: true
    },
    settings: {
      react: {
        version: 'detect'
      }
    }
  })
];
