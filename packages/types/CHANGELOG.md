# Changelog

## [1.1.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-types-v1.0.2...js-sdk-types-v1.1.0) (2024-01-18)


### Features

* introduce Context.updateVisitorProperties and Context.getConfigEntity ([0238d18](https://github.com/convertcom/javascript-sdk/commit/0238d18afa1699261c1acc30514931937dd430a1))
* optionally update in-memory visitorProperties on running experience(s)/feature(s) ([a8060d2](https://github.com/convertcom/javascript-sdk/commit/a8060d27b66d7aeb160b5cee740e6e716afcb688))

## [1.0.2](https://github.com/convertcom/javascript-sdk/compare/js-sdk-types-v1.0.1...js-sdk-types-v1.0.2) (2023-12-19)


### Bug Fixes

* require either SDk Key or data for initializing config ([4a04dc8](https://github.com/convertcom/javascript-sdk/commit/4a04dc80a226cc46b5e058ad30d3ee9ad5b2513c))

## [1.0.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-types-v1.0.0...js-sdk-types-v1.0.1) (2023-11-09)


### Bug Fixes

* correct types declarations at built time ([2141e80](https://github.com/convertcom/javascript-sdk/commit/2141e800049f9bcbf4641444b763443f196de146))

## 1.0.0 (2023-11-08)


### ⚠ BREAKING CHANGES

* extract rule-manager, bucketing-manager, and log-manager as peer dependencies

### Features

* add an independent new method for selecting matched locations ([3658bab](https://github.com/convertcom/javascript-sdk/commit/3658bab12960337a3c5fddd4b5e368b2d2736b5d))
* add country key to SegmentsData type ([63261b9](https://github.com/convertcom/javascript-sdk/commit/63261b96225a8d6aa4ebff3e040c64ca485fdd08))
* controlled bundle cache via environment variables ([449a3fe](https://github.com/convertcom/javascript-sdk/commit/449a3fe6a80f8cbaa2acf6aceb6c6b73eea387d3))
* start with tracking disabled and decide when to enable on demand ([dc58a68](https://github.com/convertcom/javascript-sdk/commit/dc58a68c4d1257e2093664a975a1d07609063da4))


### Bug Fixes

* add do-not-track type ([e5be318](https://github.com/convertcom/javascript-sdk/commit/e5be31824513df5c0214af5d667ca20b72577cdb))
* add experience integrations type ([f7ac8fd](https://github.com/convertcom/javascript-sdk/commit/f7ac8fd68f8fda78067a05f1cb41f6f203e8e36b))
* add goal types ([bb960e9](https://github.com/convertcom/javascript-sdk/commit/bb960e9bca12b871011967c46a5e84da7267fff4))
* add multipage pages to experience type ([4aa09e0](https://github.com/convertcom/javascript-sdk/commit/4aa09e0262cb766b07b22526d4668f73aba5b82f))
* add optional goal settings ([91cb3b5](https://github.com/convertcom/javascript-sdk/commit/91cb3b5ddf8580e065c8081cfa2757181ec7aefb))
* add support for boolean and function value types on rules ([84da6f4](https://github.com/convertcom/javascript-sdk/commit/84da6f46be18bdf27536100cc8f7d808feaf403f))
* add variation change type ([9cb711c](https://github.com/convertcom/javascript-sdk/commit/9cb711cdd4af84cb0ced2d191516354105190541))
* adding transaction outlier settings to experience and project config ([173a7d3](https://github.com/convertcom/javascript-sdk/commit/173a7d3715f59c44126f896de1150e1cac66df5b))
* allow native function as value for config.project.global_javascript ([a50cceb](https://github.com/convertcom/javascript-sdk/commit/a50cceb7b316116eb9044ff363e9f28ccdc0444b))
* rename npm packages ([ab54f2f](https://github.com/convertcom/javascript-sdk/commit/ab54f2ff6da4bb11caf28136117d871b48b262ef))
* skip tracking conversion event if goal already triggered ([16d9f08](https://github.com/convertcom/javascript-sdk/commit/16d9f08eae67923c3ae181e8d0c61ff0ad47acec))
* update experience integrations type ([f4e038b](https://github.com/convertcom/javascript-sdk/commit/f4e038b8b79d2057b64d107ddae132407d3ffc68))
* update goal type ([910caff](https://github.com/convertcom/javascript-sdk/commit/910caff59c63094031d1d2e85ccc978a963a107b))
* update project type ([f8000c4](https://github.com/convertcom/javascript-sdk/commit/f8000c492c82b265e826bb809477f030e6d6cc64))
* update return type for rule value ([7696be1](https://github.com/convertcom/javascript-sdk/commit/7696be160c47a9d4b0560f632d4bc49f75dc6dbe))


### Performance Improvements

* configure whether logging is enabled ([fffd9ad](https://github.com/convertcom/javascript-sdk/commit/fffd9ade05178bf5b42d11f1b0c462f94dae59c9))


### Code Refactoring

* extract rule-manager, bucketing-manager, and log-manager as peer dependencies ([2f26d4b](https://github.com/convertcom/javascript-sdk/commit/2f26d4be5cfe4ab8c8c499a2c2536368483ae74f))
