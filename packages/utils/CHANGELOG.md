# Changelog

## [2.2.3](https://github.com/convertcom/javascript-sdk/compare/js-sdk-utils-v2.2.2...js-sdk-utils-v2.2.3) (2025-09-05)


### Bug Fixes

* expose "data" property ([5df9d29](https://github.com/convertcom/javascript-sdk/commit/5df9d295af348485a2f8a1aff8c5440ef1552681))
* Improve numeric check utility ([af0d74c](https://github.com/convertcom/javascript-sdk/commit/af0d74cd027664da90d719a9b9a325dbf60ee62d))
* Improve numeric check utility ([dc49c20](https://github.com/convertcom/javascript-sdk/commit/dc49c20f4237b63ecda1ed5d230d228e8b35d2a7))

## [2.2.2](https://github.com/convertcom/javascript-sdk/compare/js-sdk-utils-v2.2.1...js-sdk-utils-v2.2.2) (2025-03-26)


### Bug Fixes

* expose "data" property ([9c5528d](https://github.com/convertcom/javascript-sdk/commit/9c5528d8989bbca80c7c4b56fa8fb1b4e1d2826d))

## [2.2.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-utils-v2.2.0...js-sdk-utils-v2.2.1) (2025-03-26)


### Bug Fixes

* handle numeric strings in comparison rules ([cbca105](https://github.com/convertcom/javascript-sdk/commit/cbca1051a314bf22d40fb95ba035474632f7018d))

## [2.2.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-utils-v2.1.0...js-sdk-utils-v2.2.0) (2024-09-26)


### Features

* enable "keepalive" on "fetch" ([8574193](https://github.com/convertcom/javascript-sdk/commit/85741934738275981d7bd2079b69e959adf93c1b))

## [2.1.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-utils-v2.0.0...js-sdk-utils-v2.1.0) (2024-07-16)


### Features

* generate rollup config per package ([1f00878](https://github.com/convertcom/javascript-sdk/commit/1f008780cc716a697e1a80bb407159b783f88a9f))


### Bug Fixes

* properly process "equals" comparison against object keys ([db2231d](https://github.com/convertcom/javascript-sdk/commit/db2231dc6dd1f6f82181c1e060ca4245242f7c0e))
* resolve missing dependencies for type declarations ([4fa0feb](https://github.com/convertcom/javascript-sdk/commit/4fa0feb2926acfc7ec82ec0b41c46b8f3753b7f1))

## [2.0.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-utils-v1.1.3...js-sdk-utils-v2.0.0) (2024-03-23)


### ⚠ BREAKING CHANGES

* use generated API config types to match the published specs at https://api.convert.com/doc/serving/

### Features

* use generated API config types to match the published specs at https://api.convert.com/doc/serving/ ([681d038](https://github.com/convertcom/javascript-sdk/commit/681d03845c2d36e303930865275677e8a37faa15))

## [1.1.3](https://github.com/convertcom/javascript-sdk/compare/js-sdk-utils-v1.1.2...js-sdk-utils-v1.1.3) (2024-03-12)


### Bug Fixes

* more robust utility for checking plain object ([c4c28bf](https://github.com/convertcom/javascript-sdk/commit/c4c28bf82765054011a170bceeaa0488cf364437))

## [1.1.2](https://github.com/convertcom/javascript-sdk/compare/js-sdk-utils-v1.1.1...js-sdk-utils-v1.1.2) (2024-02-16)


### Bug Fixes

* case-insensitive "equal" and "matches" comparisons ([b9dea18](https://github.com/convertcom/javascript-sdk/commit/b9dea189fe0e4ff52ebfa2ec547205cfe9c30304))

## [1.1.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-utils-v1.1.0...js-sdk-utils-v1.1.1) (2024-02-15)


### Bug Fixes

* less/lessEqual comparison negation logic ([8a1ed8f](https://github.com/convertcom/javascript-sdk/commit/8a1ed8f9ddfb0bf89da1619e04c9b04b3c424480))

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
