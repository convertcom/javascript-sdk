module.exports = {
  root: true,
  parser: '@typescript-eslint/parser',
  env: {
    browser: true,
    node: true,
    commonjs: true,
    mocha: true
  },
  extends: [
    'eslint:recommended',
    'plugin:@typescript-eslint/eslint-recommended',
    'plugin:@typescript-eslint/recommended',
    // Prettier plugin and recommended rules
    'plugin:prettier/recommended',
    'plugin:mocha/recommended'
  ],
  plugins: ['eslint-plugin-local-rules'],
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
  settings: {
    react: {
      version: 'detect'
    }
  }
};
