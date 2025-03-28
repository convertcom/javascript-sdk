# Changelog

## [2.3.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-experience-v2.3.0...js-sdk-experience-v2.3.1) (2025-03-26)


### Bug Fixes

* expose "data" property ([9c5528d](https://github.com/convertcom/javascript-sdk/commit/9c5528d8989bbca80c7c4b56fa8fb1b4e1d2826d))

## [2.3.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-experience-v2.2.0...js-sdk-experience-v2.3.0) (2024-11-26)


### Features

* handle audience matching options "any/all" ([e7b2da3](https://github.com/convertcom/javascript-sdk/commit/e7b2da303d053b01c81aac3e94da90d0cb3eee14))

## [2.2.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-experience-v2.1.0...js-sdk-experience-v2.2.0) (2024-07-16)


### Features

* generate rollup config per package ([1f00878](https://github.com/convertcom/javascript-sdk/commit/1f008780cc716a697e1a80bb407159b783f88a9f))
* introduce BucketingError when deciding variations ([10caaae](https://github.com/convertcom/javascript-sdk/commit/10caaae7ba586a88b6e9d02acff43b3d6481c815))


### Bug Fixes

* resolve missing dependencies for type declarations ([4fa0feb](https://github.com/convertcom/javascript-sdk/commit/4fa0feb2926acfc7ec82ec0b41c46b8f3753b7f1))

## [2.1.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-experience-v2.0.0...js-sdk-experience-v2.1.0) (2024-04-03)


### Features

* introduce optional "foceVariationId" when selecting variation(s) using DataManager ([2d32194](https://github.com/convertcom/javascript-sdk/commit/2d3219425bbb54c6ef5e30d9ba2f697cc9de2591))

## [2.0.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-experience-v1.2.0...js-sdk-experience-v2.0.0) (2024-03-23)


### ⚠ BREAKING CHANGES

* use generated API config types to match the published specs at https://api.convert.com/doc/serving/

### Features

* use generated API config types to match the published specs at https://api.convert.com/doc/serving/ ([681d038](https://github.com/convertcom/javascript-sdk/commit/681d03845c2d36e303930865275677e8a37faa15))

## [1.2.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-experience-v1.1.0...js-sdk-experience-v1.2.0) (2024-02-12)


### Features

* adding optional property for deciding whether to send tracking  event upon bucketing visitors when using ExperiencManager directly ([1bac32a](https://github.com/convertcom/javascript-sdk/commit/1bac32a2c38f15f47b4009aabec5a381c443ded9))

## [1.1.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-experience-v1.0.1...js-sdk-experience-v1.1.0) (2024-01-18)


### Features

* introduce Context.updateVisitorProperties and Context.getConfigEntity ([0238d18](https://github.com/convertcom/javascript-sdk/commit/0238d18afa1699261c1acc30514931937dd430a1))
* optionally update in-memory visitorProperties on running experience(s)/feature(s) ([a8060d2](https://github.com/convertcom/javascript-sdk/commit/a8060d27b66d7aeb160b5cee740e6e716afcb688))

## [1.0.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-experience-v1.0.0...js-sdk-experience-v1.0.1) (2023-11-09)


### Bug Fixes

* correct types declarations at built time ([2141e80](https://github.com/convertcom/javascript-sdk/commit/2141e800049f9bcbf4641444b763443f196de146))

## 1.0.0 (2023-11-08)


### Features

* controlled bundle cache via environment variables ([449a3fe](https://github.com/convertcom/javascript-sdk/commit/449a3fe6a80f8cbaa2acf6aceb6c6b73eea387d3))


### Bug Fixes

* handle segmentation audiences ([67b611a](https://github.com/convertcom/javascript-sdk/commit/67b611ae3820e82fb334c37e21e5d1a79ba113a3))
* rename npm packages ([ab54f2f](https://github.com/convertcom/javascript-sdk/commit/ab54f2ff6da4bb11caf28136117d871b48b262ef))


### Performance Improvements

* configure whether logging is enabled ([fffd9ad](https://github.com/convertcom/javascript-sdk/commit/fffd9ade05178bf5b42d11f1b0c462f94dae59c9))
