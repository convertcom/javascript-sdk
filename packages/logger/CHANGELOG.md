# Changelog

## [2.1.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-logger-v2.0.0...js-sdk-logger-v2.1.0) (2024-07-16)


### Features

* generate rollup config per package ([1f00878](https://github.com/convertcom/javascript-sdk/commit/1f008780cc716a697e1a80bb407159b783f88a9f))


### Bug Fixes

* resolve missing dependencies for type declarations ([4fa0feb](https://github.com/convertcom/javascript-sdk/commit/4fa0feb2926acfc7ec82ec0b41c46b8f3753b7f1))

## [2.0.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-logger-v1.0.1...js-sdk-logger-v2.0.0) (2024-03-23)


### ⚠ BREAKING CHANGES

* use generated API config types to match the published specs at https://api.convert.com/doc/serving/

### Features

* use generated API config types to match the published specs at https://api.convert.com/doc/serving/ ([681d038](https://github.com/convertcom/javascript-sdk/commit/681d03845c2d36e303930865275677e8a37faa15))

## [1.0.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-logger-v1.0.0...js-sdk-logger-v1.0.1) (2023-11-09)


### Bug Fixes

* correct types declarations at built time ([2141e80](https://github.com/convertcom/javascript-sdk/commit/2141e800049f9bcbf4641444b763443f196de146))

## 1.0.0 (2023-11-08)


### ⚠ BREAKING CHANGES

* extract rule-manager, bucketing-manager, and log-manager as peer dependencies

### Features

* allow changing LogLevel at run time ([3efcca5](https://github.com/convertcom/javascript-sdk/commit/3efcca5ea40213543ef44f39fdc82114059a4f20))
* controlled bundle cache via environment variables ([449a3fe](https://github.com/convertcom/javascript-sdk/commit/449a3fe6a80f8cbaa2acf6aceb6c6b73eea387d3))


### Bug Fixes

* handle optional client sdk when changing LogLevel ([f6397b0](https://github.com/convertcom/javascript-sdk/commit/f6397b0179ca7de10fc419a38cb57ff7e981a6ef))
* rename npm packages ([ab54f2f](https://github.com/convertcom/javascript-sdk/commit/ab54f2ff6da4bb11caf28136117d871b48b262ef))
* support trace() method by default for log manager ([9097fdc](https://github.com/convertcom/javascript-sdk/commit/9097fdcc295ae3afbaa545537063c838fd494e02))


### Performance Improvements

* configure whether logging is enabled ([fffd9ad](https://github.com/convertcom/javascript-sdk/commit/fffd9ade05178bf5b42d11f1b0c462f94dae59c9))


### Code Refactoring

* extract rule-manager, bucketing-manager, and log-manager as peer dependencies ([2f26d4b](https://github.com/convertcom/javascript-sdk/commit/2f26d4be5cfe4ab8c8c499a2c2536368483ae74f))
