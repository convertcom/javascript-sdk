# Changelog

## [2.1.4](https://github.com/convertcom/javascript-sdk/compare/js-sdk-api-v2.1.3...js-sdk-api-v2.1.4) (2025-09-05)


### Bug Fixes

* expose "data" property ([5df9d29](https://github.com/convertcom/javascript-sdk/commit/5df9d295af348485a2f8a1aff8c5440ef1552681))
* Improve numeric check utility ([af0d74c](https://github.com/convertcom/javascript-sdk/commit/af0d74cd027664da90d719a9b9a325dbf60ee62d))

## [2.1.3](https://github.com/convertcom/javascript-sdk/compare/js-sdk-api-v2.1.2...js-sdk-api-v2.1.3) (2025-03-26)


### Bug Fixes

* expose "data" property ([9c5528d](https://github.com/convertcom/javascript-sdk/commit/9c5528d8989bbca80c7c4b56fa8fb1b4e1d2826d))

## [2.1.2](https://github.com/convertcom/javascript-sdk/compare/js-sdk-api-v2.1.1...js-sdk-api-v2.1.2) (2025-03-26)


### Bug Fixes

* allow smaller batch size in api manager ([07e9563](https://github.com/convertcom/javascript-sdk/commit/07e95630a8ebd9fcc63f896a881e62a2f4f0a682))

## [2.1.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-api-v2.1.0...js-sdk-api-v2.1.1) (2024-11-06)


### Bug Fixes

* environment endpoints ([eb27ab2](https://github.com/convertcom/javascript-sdk/commit/eb27ab261cd5a144ca5152aefe4376ac79750b74))

## [2.1.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-api-v2.0.0...js-sdk-api-v2.1.0) (2024-07-16)


### Features

* generate rollup config per package ([1f00878](https://github.com/convertcom/javascript-sdk/commit/1f008780cc716a697e1a80bb407159b783f88a9f))
* implement SDK Key/Secret with Authorization Bearer header ([19de29d](https://github.com/convertcom/javascript-sdk/commit/19de29d8961152ab26acdd51346b60248f664bf3))
* include visitors with events when triggering event "api.queue.released" ([7c98f54](https://github.com/convertcom/javascript-sdk/commit/7c98f54082b7fa3282438c52269966f6d2b31288))
* pass environment to config endpoint ([67f8354](https://github.com/convertcom/javascript-sdk/commit/67f8354d05da00a4393239739195e3e6e090a5ef))


### Bug Fixes

* resolve missing dependencies for type declarations ([4fa0feb](https://github.com/convertcom/javascript-sdk/commit/4fa0feb2926acfc7ec82ec0b41c46b8f3753b7f1))

## [2.0.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-api-v1.0.1...js-sdk-api-v2.0.0) (2024-03-23)


### ⚠ BREAKING CHANGES

* use generated API config types to match the published specs at https://api.convert.com/doc/serving/

### Features

* use generated API config types to match the published specs at https://api.convert.com/doc/serving/ ([681d038](https://github.com/convertcom/javascript-sdk/commit/681d03845c2d36e303930865275677e8a37faa15))

## [1.0.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-api-v1.0.0...js-sdk-api-v1.0.1) (2023-11-09)


### Bug Fixes

* correct types declarations at built time ([2141e80](https://github.com/convertcom/javascript-sdk/commit/2141e800049f9bcbf4641444b763443f196de146))

## 1.0.0 (2023-11-08)


### Features

* controlled bundle cache via environment variables ([449a3fe](https://github.com/convertcom/javascript-sdk/commit/449a3fe6a80f8cbaa2acf6aceb6c6b73eea387d3))
* start with tracking disabled and decide when to enable on demand ([dc58a68](https://github.com/convertcom/javascript-sdk/commit/dc58a68c4d1257e2093664a975a1d07609063da4))


### Bug Fixes

* adjust test coverage tearget on exposed packages ([1df2831](https://github.com/convertcom/javascript-sdk/commit/1df2831bdd61cf89c6d1d7f52010b8b878a1e1e5))
* handle segmentation audiences ([67b611a](https://github.com/convertcom/javascript-sdk/commit/67b611ae3820e82fb334c37e21e5d1a79ba113a3))
* prevent tracking empty visitor events at api manager ([568e03b](https://github.com/convertcom/javascript-sdk/commit/568e03b4a2c6ca3bb6a7981e08854e818cd630be))
* rename npm packages ([ab54f2f](https://github.com/convertcom/javascript-sdk/commit/ab54f2ff6da4bb11caf28136117d871b48b262ef))


### Performance Improvements

* configure whether logging is enabled ([fffd9ad](https://github.com/convertcom/javascript-sdk/commit/fffd9ade05178bf5b42d11f1b0c462f94dae59c9))
