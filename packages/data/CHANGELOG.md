# Changelog

## [2.0.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-data-v1.2.0...js-sdk-data-v2.0.0) (2024-03-12)


### ⚠ BREAKING CHANGES

* include experience key hash bucketing

### Features

* include experience key hash bucketing ([2b7df97](https://github.com/convertcom/javascript-sdk/commit/2b7df976506666b9ef251563008c18a5a00ed7ff))

## [1.2.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-data-v1.1.0...js-sdk-data-v1.2.0) (2024-02-12)


### Features

* adding optional property for deciding whether to send tracking  event upon bucketing visitors when using ExperiencManager directly ([1bac32a](https://github.com/convertcom/javascript-sdk/commit/1bac32a2c38f15f47b4009aabec5a381c443ded9))


### Bug Fixes

* gracefully handle config server-side errors ([a88d7d3](https://github.com/convertcom/javascript-sdk/commit/a88d7d395d98c850b6af002237d3128f97cad89a))

## [1.1.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-data-v1.0.1...js-sdk-data-v1.1.0) (2024-01-18)


### Features

* introduce Context.updateVisitorProperties and Context.getConfigEntity ([0238d18](https://github.com/convertcom/javascript-sdk/commit/0238d18afa1699261c1acc30514931937dd430a1))
* optionally update in-memory visitorProperties on running experience(s)/feature(s) ([a8060d2](https://github.com/convertcom/javascript-sdk/commit/a8060d27b66d7aeb160b5cee740e6e716afcb688))

## [1.0.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-data-v1.0.0...js-sdk-data-v1.0.1) (2023-11-09)


### Bug Fixes

* correct types declarations at built time ([2141e80](https://github.com/convertcom/javascript-sdk/commit/2141e800049f9bcbf4641444b763443f196de146))

## 1.0.0 (2023-11-08)


### ⚠ BREAKING CHANGES

* extract segments-manager as peer dependency

### Features

* add an independent new method for selecting matched locations ([3658bab](https://github.com/convertcom/javascript-sdk/commit/3658bab12960337a3c5fddd4b5e368b2d2736b5d))
* controlled bundle cache via environment variables ([449a3fe](https://github.com/convertcom/javascript-sdk/commit/449a3fe6a80f8cbaa2acf6aceb6c6b73eea387d3))
* expose a separate method for matching location/audience rules on data manager ([5a3b585](https://github.com/convertcom/javascript-sdk/commit/5a3b5850c68222f52c7624d14a5fd388982fab4c))
* introducing location activated/deactivated events ([8e4cc9d](https://github.com/convertcom/javascript-sdk/commit/8e4cc9dfaeea545ee7480062d911a59fbfd3ada4))
* trigger events on matching locations, audiences, and segments ([3a6e4fe](https://github.com/convertcom/javascript-sdk/commit/3a6e4fe84a91073ba58d149e5609c8bac15ad085))


### Bug Fixes

* adjust test coverage tearget on exposed packages ([1df2831](https://github.com/convertcom/javascript-sdk/commit/1df2831bdd61cf89c6d1d7f52010b8b878a1e1e5))
* always match audience if empty ([46dcd2a](https://github.com/convertcom/javascript-sdk/commit/46dcd2a38ecf84812c4167e222bdbd722908631c))
* force environment check ([9cbc838](https://github.com/convertcom/javascript-sdk/commit/9cbc8388e7023c223f5be0a3f289647009292096))
* force string experience/variation Id when sending bucketing events ([684412e](https://github.com/convertcom/javascript-sdk/commit/684412e70a32804ddfb02195405350623259054d))
* handle empty audiences, segmentations, or locations per experience as it means no restrictions ([63bafba](https://github.com/convertcom/javascript-sdk/commit/63bafba34f2a3419c0456e046b22bd5771fadfbb))
* handle numeric id lookup on entities for the data manager ([5af4109](https://github.com/convertcom/javascript-sdk/commit/5af4109639b0881aeea566c20cf78fb2cf4375c9))
* handle segmentation audiences ([67b611a](https://github.com/convertcom/javascript-sdk/commit/67b611ae3820e82fb334c37e21e5d1a79ba113a3))
* make it clear that using a custom dataStore is optional ([8a2b162](https://github.com/convertcom/javascript-sdk/commit/8a2b16232d715831c28fc7cde0ba1b592573960a))
* merge existing bucketing and segments when writing to data store ([4a6f0d4](https://github.com/convertcom/javascript-sdk/commit/4a6f0d458e6192028b027df6560726062d6b8562))
* properly fallback to site_area if experience locations is present but emty ([b48c559](https://github.com/convertcom/javascript-sdk/commit/b48c55944a778857813c6fa44e163bbeabf70d2a))
* rename npm packages ([ab54f2f](https://github.com/convertcom/javascript-sdk/commit/ab54f2ff6da4bb11caf28136117d871b48b262ef))
* skip environment check if not supported yet ([a502604](https://github.com/convertcom/javascript-sdk/commit/a5026041d85dcb5174a494bdab0d9b81af3d13ed))
* skip environment check on experiences if empty array ([9138e7e](https://github.com/convertcom/javascript-sdk/commit/9138e7e2bb8f00a79bc210654ab6839914637590))
* skip tracking conversion event if goal already triggered ([16d9f08](https://github.com/convertcom/javascript-sdk/commit/16d9f08eae67923c3ae181e8d0c61ff0ad47acec))
* validate custom segments against audiences of type segmentation ([f3de93c](https://github.com/convertcom/javascript-sdk/commit/f3de93c3602cf712a1a63accca09bc863801f76f))


### Performance Improvements

* configure whether logging is enabled ([fffd9ad](https://github.com/convertcom/javascript-sdk/commit/fffd9ade05178bf5b42d11f1b0c462f94dae59c9))


### Code Refactoring

* extract segments-manager as peer dependency ([f570fa0](https://github.com/convertcom/javascript-sdk/commit/f570fa009b6a5f6de5cd728ab102db96f45ba0c8))
