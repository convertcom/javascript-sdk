// Wrapper to force ESM import instead of browser UMD bundle
// Re-export everything from the ESM build
export * from '../../../../packages/js-sdk/lib/index.mjs';
export { default } from '../../../../packages/js-sdk/lib/index.mjs';
