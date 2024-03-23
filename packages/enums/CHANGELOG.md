# Changelog

## [2.0.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-enums-v1.1.0...js-sdk-enums-v2.0.0) (2024-03-23)


### ⚠ BREAKING CHANGES

* use generated API config types to match the published specs at https://api.convert.com/doc/serving/

### Features

* use generated API config types to match the published specs at https://api.convert.com/doc/serving/ ([681d038](https://github.com/convertcom/javascript-sdk/commit/681d03845c2d36e303930865275677e8a37faa15))

## [1.1.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-enums-v1.0.1...js-sdk-enums-v1.1.0) (2024-02-23)


### Features

* introduce "Context.getConfigEntityById()" ([58ce097](https://github.com/convertcom/javascript-sdk/commit/58ce097f0bf048825d010a7ccc93225854311380))

## [1.0.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-enums-v1.0.0...js-sdk-enums-v1.0.1) (2023-11-09)


### Bug Fixes

* correct types declarations at built time ([2141e80](https://github.com/convertcom/javascript-sdk/commit/2141e800049f9bcbf4641444b763443f196de146))

## 1.0.0 (2023-11-08)


### ⚠ BREAKING CHANGES

* rename system events
* extract rule-manager, bucketing-manager, and log-manager as peer dependencies

### Features

* add country key to SegmentsData type ([63261b9](https://github.com/convertcom/javascript-sdk/commit/63261b96225a8d6aa4ebff3e040c64ca485fdd08))
* add country key to SegmentsKeys enum ([cf6d51e](https://github.com/convertcom/javascript-sdk/commit/cf6d51e513e79b31b6f78326b720b22d14d34d03))
* controlled bundle cache via environment variables ([449a3fe](https://github.com/convertcom/javascript-sdk/commit/449a3fe6a80f8cbaa2acf6aceb6c6b73eea387d3))
* favor sendBeacon over fetch for sending tracking events ([c6418bb](https://github.com/convertcom/javascript-sdk/commit/c6418bb6a261875467913de923370a1263409333))
* introducing location activated/deactivated events ([8e4cc9d](https://github.com/convertcom/javascript-sdk/commit/8e4cc9dfaeea545ee7480062d911a59fbfd3ada4))
* trigger events on matching locations, audiences, and segments ([3a6e4fe](https://github.com/convertcom/javascript-sdk/commit/3a6e4fe84a91073ba58d149e5609c8bac15ad085))


### Bug Fixes

* add do-not-track type ([e5be318](https://github.com/convertcom/javascript-sdk/commit/e5be31824513df5c0214af5d667ca20b72577cdb))
* add edge browser type ([d8b47b8](https://github.com/convertcom/javascript-sdk/commit/d8b47b8eb8ddd7ea04a0e8919c432aca14f8adf6))
* add experience integrations type ([f7ac8fd](https://github.com/convertcom/javascript-sdk/commit/f7ac8fd68f8fda78067a05f1cb41f6f203e8e36b))
* add goal types ([bb960e9](https://github.com/convertcom/javascript-sdk/commit/bb960e9bca12b871011967c46a5e84da7267fff4))
* add optional goal settings ([91cb3b5](https://github.com/convertcom/javascript-sdk/commit/91cb3b5ddf8580e065c8081cfa2757181ec7aefb))
* rename npm packages ([ab54f2f](https://github.com/convertcom/javascript-sdk/commit/ab54f2ff6da4bb11caf28136117d871b48b262ef))
* skip tracking conversion event if goal already triggered ([16d9f08](https://github.com/convertcom/javascript-sdk/commit/16d9f08eae67923c3ae181e8d0c61ff0ad47acec))
* update browser type enum ([bbdde28](https://github.com/convertcom/javascript-sdk/commit/bbdde28d24fd12246c21af68e846e622533a0674))
* update do-not-track type ([297637f](https://github.com/convertcom/javascript-sdk/commit/297637f1e41250298d9f690633e52973ddb37ff5))
* update experience integrations type ([f4e038b](https://github.com/convertcom/javascript-sdk/commit/f4e038b8b79d2057b64d107ddae132407d3ffc68))
* update goal data key enum ([19a7a1e](https://github.com/convertcom/javascript-sdk/commit/19a7a1ead09a4094d1230169e95a41b7559aaa36))
* update goal type ([910caff](https://github.com/convertcom/javascript-sdk/commit/910caff59c63094031d1d2e85ccc978a963a107b))
* update project type ([f8000c4](https://github.com/convertcom/javascript-sdk/commit/f8000c492c82b265e826bb809477f030e6d6cc64))
* update variation status enum ([8942883](https://github.com/convertcom/javascript-sdk/commit/8942883dca57d5d566af607ce28f897463d6193a))


### Performance Improvements

* configure whether logging is enabled ([fffd9ad](https://github.com/convertcom/javascript-sdk/commit/fffd9ade05178bf5b42d11f1b0c462f94dae59c9))


### Code Refactoring

* extract rule-manager, bucketing-manager, and log-manager as peer dependencies ([2f26d4b](https://github.com/convertcom/javascript-sdk/commit/2f26d4be5cfe4ab8c8c499a2c2536368483ae74f))
* rename system events ([0244a21](https://github.com/convertcom/javascript-sdk/commit/0244a21274d52e264dadd1923d83e7d1a74f6064))
