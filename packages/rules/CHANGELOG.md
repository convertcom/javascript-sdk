# Changelog

## [1.0.2](https://github.com/convertcom/javascript-sdk/compare/js-sdk-rules-v1.0.1...js-sdk-rules-v1.0.2) (2024-03-12)


### Bug Fixes

* more robust utility for checking plain object ([c4c28bf](https://github.com/convertcom/javascript-sdk/commit/c4c28bf82765054011a170bceeaa0488cf364437))

## [1.0.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-rules-v1.0.0...js-sdk-rules-v1.0.1) (2023-11-09)


### Bug Fixes

* correct types declarations at built time ([2141e80](https://github.com/convertcom/javascript-sdk/commit/2141e800049f9bcbf4641444b763443f196de146))

## 1.0.0 (2023-11-08)


### ⚠ BREAKING CHANGES

* extract rule-manager, bucketing-manager, and log-manager as peer dependencies

### Features

* controlled bundle cache via environment variables ([449a3fe](https://github.com/convertcom/javascript-sdk/commit/449a3fe6a80f8cbaa2acf6aceb6c6b73eea387d3))
* extend rule manager ([3287c3b](https://github.com/convertcom/javascript-sdk/commit/3287c3bd9dcfc059d3131925b8d4fc03ac6a7092))


### Bug Fixes

* adjust test coverage tearget on exposed packages ([1df2831](https://github.com/convertcom/javascript-sdk/commit/1df2831bdd61cf89c6d1d7f52010b8b878a1e1e5))
* allow rules without key property in case of using a data source ([21d2956](https://github.com/convertcom/javascript-sdk/commit/21d29564130bbb840ce1042ff0d72e3a7be17448))
* handle segmentation audiences ([67b611a](https://github.com/convertcom/javascript-sdk/commit/67b611ae3820e82fb334c37e21e5d1a79ba113a3))
* match javascript condition rules against evaluated value instead of rule value ([d301a63](https://github.com/convertcom/javascript-sdk/commit/d301a636d1ef4aa4f606f91c29eea27063f0e7e9))
* rename npm packages ([ab54f2f](https://github.com/convertcom/javascript-sdk/commit/ab54f2ff6da4bb11caf28136117d871b48b262ef))


### Performance Improvements

* configure whether logging is enabled ([fffd9ad](https://github.com/convertcom/javascript-sdk/commit/fffd9ade05178bf5b42d11f1b0c462f94dae59c9))


### Code Refactoring

* extract rule-manager, bucketing-manager, and log-manager as peer dependencies ([2f26d4b](https://github.com/convertcom/javascript-sdk/commit/2f26d4be5cfe4ab8c8c499a2c2536368483ae74f))
