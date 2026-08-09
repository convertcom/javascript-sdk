# Changelog

## [2.0.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-cloudflare-v2.0.0...js-sdk-cloudflare-v2.0.1) (2026-08-09)


### Bug Fixes

* **build:** bump rollup-plugin-typescript2 to 0.37 for picomatch 2.3.2 ([15633da](https://github.com/convertcom/javascript-sdk/commit/15633daef67aeb13f4f8f0b48b4d3e9df3c30264))

## [2.0.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-cloudflare-v1.1.1...js-sdk-cloudflare-v2.0.0) (2026-07-24)


### ⚠ BREAKING CHANGES

* **bucketing:** bucketing layout is now selected by experience.version; experiences at version > 11 use the new anchored layout instead of the packed cumulative walk.

### Features

* **bucketing:** anchored (ramping) bucketing layout selected by experience version ([03bfb5e](https://github.com/convertcom/javascript-sdk/commit/03bfb5ef5d0568658d985ed962dd8f36d5d62214))

## [1.1.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-cloudflare-v1.1.0...js-sdk-cloudflare-v1.1.1) (2026-06-05)


### Bug Fixes

* **js-sdk:** pick up Web Worker runtime detection fix from js-sdk-utils ([7d47d87](https://github.com/convertcom/javascript-sdk/commit/7d47d87a82b6797e7683205a26a5f60589d3c06e))

## [1.1.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-cloudflare-v1.0.0...js-sdk-cloudflare-v1.1.0) (2026-04-06)


### Features

* add Cloudflare Workers utility package and demo ([00c2bb6](https://github.com/convertcom/javascript-sdk/commit/00c2bb627bb3f4e852a59e455da5c9787010085b))


### Bug Fixes

* expose "data" property ([5df9d29](https://github.com/convertcom/javascript-sdk/commit/5df9d295af348485a2f8a1aff8c5440ef1552681))
* expose "data" property ([9c5528d](https://github.com/convertcom/javascript-sdk/commit/9c5528d8989bbca80c7c4b56fa8fb1b4e1d2826d))
* Improve numeric check utility ([af0d74c](https://github.com/convertcom/javascript-sdk/commit/af0d74cd027664da90d719a9b9a325dbf60ee62d))
