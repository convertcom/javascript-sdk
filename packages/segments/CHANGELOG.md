# Changelog

## [2.0.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-segments-v1.1.0...js-sdk-segments-v2.0.0) (2024-03-23)


### ⚠ BREAKING CHANGES

* use generated API config types to match the published specs at https://api.convert.com/doc/serving/

### Features

* use generated API config types to match the published specs at https://api.convert.com/doc/serving/ ([681d038](https://github.com/convertcom/javascript-sdk/commit/681d03845c2d36e303930865275677e8a37faa15))

## [1.1.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-segments-v1.0.1...js-sdk-segments-v1.1.0) (2024-01-18)


### Features

* introduce Context.updateVisitorProperties and Context.getConfigEntity ([0238d18](https://github.com/convertcom/javascript-sdk/commit/0238d18afa1699261c1acc30514931937dd430a1))


### Bug Fixes

* run custom segments against data store if not found in memory ([98ae120](https://github.com/convertcom/javascript-sdk/commit/98ae1201cc5e69d88656d13dfb9adac096a4c02e))

## [1.0.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-segments-v1.0.0...js-sdk-segments-v1.0.1) (2023-11-09)


### Bug Fixes

* correct types declarations at built time ([2141e80](https://github.com/convertcom/javascript-sdk/commit/2141e800049f9bcbf4641444b763443f196de146))

## 1.0.0 (2023-11-08)


### ⚠ BREAKING CHANGES

* extract segments-manager as peer dependency

### Bug Fixes

* merge existing bucketing and segments when writing to data store ([4a6f0d4](https://github.com/convertcom/javascript-sdk/commit/4a6f0d458e6192028b027df6560726062d6b8562))
* support trace() method by default for log manager ([9097fdc](https://github.com/convertcom/javascript-sdk/commit/9097fdcc295ae3afbaa545537063c838fd494e02))
* validate custom segments against audiences of type segmentation ([f3de93c](https://github.com/convertcom/javascript-sdk/commit/f3de93c3602cf712a1a63accca09bc863801f76f))


### Performance Improvements

* configure whether logging is enabled ([fffd9ad](https://github.com/convertcom/javascript-sdk/commit/fffd9ade05178bf5b42d11f1b0c462f94dae59c9))


### Code Refactoring

* extract segments-manager as peer dependency ([f570fa0](https://github.com/convertcom/javascript-sdk/commit/f570fa009b6a5f6de5cd728ab102db96f45ba0c8))
