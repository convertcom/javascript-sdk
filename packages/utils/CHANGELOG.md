# Changelog

## [1.1.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-utils-v1.0.2...js-sdk-utils-v1.1.0) (2024-01-18)


### Features

* introduce Context.updateVisitorProperties and Context.getConfigEntity ([0238d18](https://github.com/convertcom/javascript-sdk/commit/0238d18afa1699261c1acc30514931937dd430a1))

## [1.0.2](https://github.com/convertcom/javascript-sdk/compare/js-sdk-utils-v1.0.1...js-sdk-utils-v1.0.2) (2023-11-19)


### Bug Fixes

* support edge runtime ([31056bc](https://github.com/convertcom/javascript-sdk/commit/31056bc38db7370e673fc693b5446dce8abf30d1))

## [1.0.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-utils-v1.0.0...js-sdk-utils-v1.0.1) (2023-11-09)


### Bug Fixes

* correct types declarations at built time ([2141e80](https://github.com/convertcom/javascript-sdk/commit/2141e800049f9bcbf4641444b763443f196de146))

## 1.0.0 (2023-11-08)


### ⚠ BREAKING CHANGES

* extract rule-manager, bucketing-manager, and log-manager as peer dependencies

### Features

* controlled bundle cache via environment variables ([449a3fe](https://github.com/convertcom/javascript-sdk/commit/449a3fe6a80f8cbaa2acf6aceb6c6b73eea387d3))
* extend rule manager ([3287c3b](https://github.com/convertcom/javascript-sdk/commit/3287c3bd9dcfc059d3131925b8d4fc03ac6a7092))
* favor sendBeacon over fetch for sending tracking events ([c6418bb](https://github.com/convertcom/javascript-sdk/commit/c6418bb6a261875467913de923370a1263409333))


### Bug Fixes

* expose serialize helper ([7568f01](https://github.com/convertcom/javascript-sdk/commit/7568f01119f7144e139cc81f4427e41de6b7eb14))
* rename npm packages ([ab54f2f](https://github.com/convertcom/javascript-sdk/commit/ab54f2ff6da4bb11caf28136117d871b48b262ef))


### Performance Improvements

* configure whether logging is enabled ([fffd9ad](https://github.com/convertcom/javascript-sdk/commit/fffd9ade05178bf5b42d11f1b0c462f94dae59c9))


### Code Refactoring

* extract rule-manager, bucketing-manager, and log-manager as peer dependencies ([2f26d4b](https://github.com/convertcom/javascript-sdk/commit/2f26d4be5cfe4ab8c8c499a2c2536368483ae74f))
