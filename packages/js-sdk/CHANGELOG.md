# Changelog

## [2.2.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v2.1.0...js-sdk-v2.2.0) (2024-02-12)


### Features

* adding optional property for deciding whether to send tracking  event upon bucketing visitors when using ExperiencManager directly ([1bac32a](https://github.com/convertcom/javascript-sdk/commit/1bac32a2c38f15f47b4009aabec5a381c443ded9))


### Bug Fixes

* gracefully handle config server-side errors ([a88d7d3](https://github.com/convertcom/javascript-sdk/commit/a88d7d395d98c850b6af002237d3128f97cad89a))

## [2.1.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v2.0.3...js-sdk-v2.1.0) (2024-01-18)


### Features

* introduce Context.updateVisitorProperties and Context.getConfigEntity ([0238d18](https://github.com/convertcom/javascript-sdk/commit/0238d18afa1699261c1acc30514931937dd430a1))
* optionally update in-memory visitorProperties on running experience(s)/feature(s) ([a8060d2](https://github.com/convertcom/javascript-sdk/commit/a8060d27b66d7aeb160b5cee740e6e716afcb688))


### Bug Fixes

* always include report segments with visitorProperties for rule matching ([e769717](https://github.com/convertcom/javascript-sdk/commit/e7697173791fdaffe44b23a1ad5dd1194c0997c7))
* filter report segments on Context.setDefaultSegments ([e1bbd2f](https://github.com/convertcom/javascript-sdk/commit/e1bbd2fba69f22704dd8d83c8a58d53666d45651))
* rename Context.setCustomSegments to Context.runCustomSegments ([630d134](https://github.com/convertcom/javascript-sdk/commit/630d134b1f9024795b4d9b5b53ede46ffb071b1e))

## [2.0.3](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v2.0.2...js-sdk-v2.0.3) (2023-12-19)


### Bug Fixes

* require either SDk Key or data for initializing config ([eb301ad](https://github.com/convertcom/javascript-sdk/commit/eb301ad4c93d790ea565c13f3281258e1c4daa89))

## [2.0.2](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v2.0.1...js-sdk-v2.0.2) (2023-11-19)


### Bug Fixes

* support edge runtime ([796cc43](https://github.com/convertcom/javascript-sdk/commit/796cc43b207d4d04a44ab93d1a35d4a672f7d9ec))

## [2.0.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v2.0.0...js-sdk-v2.0.1) (2023-11-09)


### Bug Fixes

* correct types declarations at built time ([2141e80](https://github.com/convertcom/javascript-sdk/commit/2141e800049f9bcbf4641444b763443f196de146))

## [2.0.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v1.2.1...js-sdk-v2.0.0) (2023-11-08)


### ⚠ BREAKING CHANGES

* extract js-sdk-api, js-sdk-rule, js-sdk-bucketing, js-sdk-event, js-sdk-data, js-sdk-experience, js-sdk-segments, js-sdk-logger, js-sdk-enums, js-sdk-types, and js-sdk-utils as peer dependencies

### Code Refactoring

* extract js-sdk-api, js-sdk-rule, js-sdk-bucketing, js-sdk-event, js-sdk-data, js-sdk-experience, js-sdk-segments, js-sdk-logger, js-sdk-enums, js-sdk-types, and js-sdk-utils as peer dependencies ([27f9a57](https://github.com/convertcom/javascript-sdk/commit/27f9a57221e5619ea185edcf657db81d4cdf0fc6))

## [1.2.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v1.2.0...js-sdk-v1.2.1) (2023-10-13)


### Bug Fixes

* allow optional environment per experience when bucketing visitors ([d2a45e9](https://github.com/convertcom/javascript-sdk/commit/d2a45e99537221d8bbf6762f52f1124e1099b8bf))
* handle the case of no audience restrictions when bucketing visitors ([81fe7bb](https://github.com/convertcom/javascript-sdk/commit/81fe7bbf87e7c719cd6ffa62881a0b7a300bb33e))
* only show invalid dataStore error when provided ([204127b](https://github.com/convertcom/javascript-sdk/commit/204127bb9f0af5cedcac41a63c72345c7d3a48fc))
* skip tracking conversion event if goal already triggered ([3dc4656](https://github.com/convertcom/javascript-sdk/commit/3dc465613db7ee61fe612035895b86790b1e8e74))

## [1.2.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v1.1.0...js-sdk-v1.2.0) (2023-04-09)


### Features

* Initial SDK release ([fda7b98](https://github.com/convertcom/javascript-sdk/commit/fda7b983664dcebe7e37e1645e2eee21ae4fe743))

## 1.0.0 (2023-04-07)


### Features

* Initial SDK release ([fda7b98](https://github.com/convertcom/javascript-sdk/commit/fda7b983664dcebe7e37e1645e2eee21ae4fe743))
