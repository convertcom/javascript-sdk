const {FlatCompat} = require('@eslint/eslintrc');
const js = require('@eslint/js');

const compat = new FlatCompat({
  baseDirectory: __dirname,
  resolvePluginsRelativeTo: __dirname,
  recommendedConfig: js.configs.recommended
});

module.exports = [
  {
    files: ['**/*.ts', '**/*.tsx'],
    ignores: ['.yarn', '.vscode', '.github', 'node_modules']
  },
  ...compat.extends(
    'eslint:recommended',
    'plugin:@typescript-eslint/eslint-recommended',
    'plugin:@typescript-eslint/recommended',
    // Prettier plugin and recommended rules
    'plugin:prettier/recommended',
    'plugin:mocha/recommended'
  ),
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
