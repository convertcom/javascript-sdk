# Changelog

## [4.0.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-bucketing-v4.0.0...js-sdk-bucketing-v4.0.1) (2026-08-09)


### Bug Fixes

* **build:** bump rollup-plugin-typescript2 to 0.37 for picomatch 2.3.2 ([15633da](https://github.com/convertcom/javascript-sdk/commit/15633daef67aeb13f4f8f0b48b4d3e9df3c30264))
* **deps:** drop build-generated peerDependency churn from the diff ([4b6bbdd](https://github.com/convertcom/javascript-sdk/commit/4b6bbdda5059bc6d9b4163f8a0bbc7e613c6e5ea))

## [4.0.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-bucketing-v3.1.3...js-sdk-bucketing-v4.0.0) (2026-07-24)


### ⚠ BREAKING CHANGES

* **bucketing:** bucketing layout is now selected by experience.version; experiences at version > 11 use the new anchored layout instead of the packed cumulative walk.

### Features

* **bucketing:** anchored (ramping) bucketing layout selected by experience version ([03bfb5e](https://github.com/convertcom/javascript-sdk/commit/03bfb5ef5d0568658d985ed962dd8f36d5d62214))
* **bucketing:** anchored layout (contract v12) — JS SDK reference impl ([24ec631](https://github.com/convertcom/javascript-sdk/commit/24ec631373d969c47a33d3c94334dd22cbc50699))
* Full Stack v12 — safe traffic ramp-up, variation preview & experiment exclusion ([a0f4ac4](https://github.com/convertcom/javascript-sdk/commit/a0f4ac4f1583b435323fd43947a21f616343d67c))

## [3.1.3](https://github.com/convertcom/javascript-sdk/compare/js-sdk-bucketing-v3.1.2...js-sdk-bucketing-v3.1.3) (2026-06-05)


### Bug Fixes

* **js-sdk:** pick up Web Worker runtime detection fix from js-sdk-utils ([7d47d87](https://github.com/convertcom/javascript-sdk/commit/7d47d87a82b6797e7683205a26a5f60589d3c06e))

## [3.1.2](https://github.com/convertcom/javascript-sdk/compare/js-sdk-bucketing-v3.1.1...js-sdk-bucketing-v3.1.2) (2025-09-05)


### Bug Fixes

* expose "data" property ([5df9d29](https://github.com/convertcom/javascript-sdk/commit/5df9d295af348485a2f8a1aff8c5440ef1552681))
* Improve numeric check utility ([af0d74c](https://github.com/convertcom/javascript-sdk/commit/af0d74cd027664da90d719a9b9a325dbf60ee62d))

## [3.1.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-bucketing-v3.1.0...js-sdk-bucketing-v3.1.1) (2025-03-26)


### Bug Fixes

* expose "data" property ([9c5528d](https://github.com/convertcom/javascript-sdk/commit/9c5528d8989bbca80c7c4b56fa8fb1b4e1d2826d))

## [3.1.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-bucketing-v3.0.0...js-sdk-bucketing-v3.1.0) (2024-07-16)


### Features

* generate rollup config per package ([1f00878](https://github.com/convertcom/javascript-sdk/commit/1f008780cc716a697e1a80bb407159b783f88a9f))
* return bucketing allocation upon deciding variation ([06fb25f](https://github.com/convertcom/javascript-sdk/commit/06fb25fb56477dfbd55e46af5d38dd53316cfdc9))


### Bug Fixes

* resolve missing dependencies for type declarations ([4fa0feb](https://github.com/convertcom/javascript-sdk/commit/4fa0feb2926acfc7ec82ec0b41c46b8f3753b7f1))

## [3.0.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-bucketing-v2.0.0...js-sdk-bucketing-v3.0.0) (2024-03-23)


### ⚠ BREAKING CHANGES

* use generated API config types to match the published specs at https://api.convert.com/doc/serving/

### Features

* use generated API config types to match the published specs at https://api.convert.com/doc/serving/ ([681d038](https://github.com/convertcom/javascript-sdk/commit/681d03845c2d36e303930865275677e8a37faa15))

## [2.0.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-bucketing-v1.0.1...js-sdk-bucketing-v2.0.0) (2024-03-12)


### ⚠ BREAKING CHANGES

* include experience key hash bucketing

### Features

* include experience key hash bucketing ([2b7df97](https://github.com/convertcom/javascript-sdk/commit/2b7df976506666b9ef251563008c18a5a00ed7ff))

## [1.0.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-bucketing-v1.0.0...js-sdk-bucketing-v1.0.1) (2023-11-09)


### Bug Fixes

* correct types declarations at built time ([2141e80](https://github.com/convertcom/javascript-sdk/commit/2141e800049f9bcbf4641444b763443f196de146))

## 1.0.0 (2023-11-08)


### ⚠ BREAKING CHANGES

* extract rule-manager, bucketing-manager, and log-manager as peer dependencies

### Features

* controlled bundle cache via environment variables ([449a3fe](https://github.com/convertcom/javascript-sdk/commit/449a3fe6a80f8cbaa2acf6aceb6c6b73eea387d3))
* expose generic method for generating seeded hash ([bb32531](https://github.com/convertcom/javascript-sdk/commit/bb32531f0d92ad3dc7169aa5bf057f9bd20b26a8))


### Bug Fixes

* handle segmentation audiences ([67b611a](https://github.com/convertcom/javascript-sdk/commit/67b611ae3820e82fb334c37e21e5d1a79ba113a3))
* rename npm packages ([ab54f2f](https://github.com/convertcom/javascript-sdk/commit/ab54f2ff6da4bb11caf28136117d871b48b262ef))


### Performance Improvements

* configure whether logging is enabled ([fffd9ad](https://github.com/convertcom/javascript-sdk/commit/fffd9ade05178bf5b42d11f1b0c462f94dae59c9))


### Code Refactoring

* extract rule-manager, bucketing-manager, and log-manager as peer dependencies ([2f26d4b](https://github.com/convertcom/javascript-sdk/commit/2f26d4be5cfe4ab8c8c499a2c2536368483ae74f))
