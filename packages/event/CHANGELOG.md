# Changelog

## [2.1.2](https://github.com/convertcom/javascript-sdk/compare/js-sdk-event-v2.1.1...js-sdk-event-v2.1.2) (2025-03-26)


### Bug Fixes

* expose "data" property ([9c5528d](https://github.com/convertcom/javascript-sdk/commit/9c5528d8989bbca80c7c4b56fa8fb1b4e1d2826d))

## [2.1.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-event-v2.1.0...js-sdk-event-v2.1.1) (2024-11-26)


### Bug Fixes

* handle exceptions at event listeners ([6b5067a](https://github.com/convertcom/javascript-sdk/commit/6b5067aafbe8c4bc3dd30b06a016aeb775f0f664))

## [2.1.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-event-v2.0.0...js-sdk-event-v2.1.0) (2024-07-16)


### Features

* generate rollup config per package ([1f00878](https://github.com/convertcom/javascript-sdk/commit/1f008780cc716a697e1a80bb407159b783f88a9f))


### Bug Fixes

* resolve missing dependencies for type declarations ([4fa0feb](https://github.com/convertcom/javascript-sdk/commit/4fa0feb2926acfc7ec82ec0b41c46b8f3753b7f1))

## [2.0.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-event-v1.0.2...js-sdk-event-v2.0.0) (2024-03-23)


### ⚠ BREAKING CHANGES

* use generated API config types to match the published specs at https://api.convert.com/doc/serving/

### Features

* use generated API config types to match the published specs at https://api.convert.com/doc/serving/ ([681d038](https://github.com/convertcom/javascript-sdk/commit/681d03845c2d36e303930865275677e8a37faa15))

## [1.0.2](https://github.com/convertcom/javascript-sdk/compare/js-sdk-event-v1.0.1...js-sdk-event-v1.0.2) (2024-02-16)


### Bug Fixes

* ignore extended properties when firing events by the EventManager ([f623b9b](https://github.com/convertcom/javascript-sdk/commit/f623b9bdece54d50aa21da76b7c99ea33e632094))

## [1.0.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-event-v1.0.0...js-sdk-event-v1.0.1) (2023-11-09)


### Bug Fixes

* correct types declarations at built time ([2141e80](https://github.com/convertcom/javascript-sdk/commit/2141e800049f9bcbf4641444b763443f196de146))

## 1.0.0 (2023-11-08)


### ⚠ BREAKING CHANGES

* extract event-manager and data-store-manager as peer dependencies

### Features

* controlled bundle cache via environment variables ([449a3fe](https://github.com/convertcom/javascript-sdk/commit/449a3fe6a80f8cbaa2acf6aceb6c6b73eea387d3))


### Bug Fixes

* enable callback arguments in event manager interface ([103952c](https://github.com/convertcom/javascript-sdk/commit/103952caa092a8bb3b05989a3a20af898393b973))
* enable callback arguments in event manager interface ([064a2d3](https://github.com/convertcom/javascript-sdk/commit/064a2d3e0ec436bda7c5c2c0d4ec6679049fd148))
* handle segmentation audiences ([67b611a](https://github.com/convertcom/javascript-sdk/commit/67b611ae3820e82fb334c37e21e5d1a79ba113a3))
* optional arguments on firing events ([f404e19](https://github.com/convertcom/javascript-sdk/commit/f404e19faeee251648bfc184c00a96c32a6ba558))
* rename npm packages ([ab54f2f](https://github.com/convertcom/javascript-sdk/commit/ab54f2ff6da4bb11caf28136117d871b48b262ef))


### Performance Improvements

* configure whether logging is enabled ([fffd9ad](https://github.com/convertcom/javascript-sdk/commit/fffd9ade05178bf5b42d11f1b0c462f94dae59c9))


### Code Refactoring

* extract event-manager and data-store-manager as peer dependencies ([096fc68](https://github.com/convertcom/javascript-sdk/commit/096fc6800663886b9bbe57a8864a60b16e1138b6))
