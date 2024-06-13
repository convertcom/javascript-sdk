import globals from 'globals';
import eslintParser from '@typescript-eslint/parser';
import eslintPluginLocalRules from 'eslint-plugin-local-rules';
import eslintPluginTypescript from 'typescript-eslint';
import eslintPluginPrettierRecommended from 'eslint-plugin-prettier/recommended';
import eslintPluginMocha from 'eslint-plugin-mocha';
import eslintJs from '@eslint/js';

export default [
  eslintJs.configs.recommended,
  eslintPluginTypescript.configs.recommended,
  eslintPluginPrettierRecommended,
  eslintPluginMocha.configs.flat.recommended,
  {
    languageOptions: {
      parser: eslintParser,
      globals: {
        ...globals.browser,
        ...globals.node,
        ...globals.commonjs,
        ...globals.mocha
      }
    },
    plugins: {
      'eslint-plugin-local-rules': eslintPluginLocalRules
    },
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
      'local-rules/log-manager': 'error'
    },
    files: ['**/*.ts', '**/*.tsx'],
    ignores: ['.next', 'node_modules']
  }
];
