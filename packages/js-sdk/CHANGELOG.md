# Changelog

## [4.2.2](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v4.2.1...js-sdk-v4.2.2) (2024-11-06)


### Bug Fixes

* environment endpoints ([eb27ab2](https://github.com/convertcom/javascript-sdk/commit/eb27ab261cd5a144ca5152aefe4376ac79750b74))

## [4.2.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v4.2.0...js-sdk-v4.2.1) (2024-11-04)


### Bug Fixes

* handle forced variation ([0e75106](https://github.com/convertcom/javascript-sdk/commit/0e75106955fdfa4a1641abc426ab4590d3791a18))

## [4.2.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v4.1.0...js-sdk-v4.2.0) (2024-09-26)


### Features

* promisify Context.releaseQueues() ([8a13ba9](https://github.com/convertcom/javascript-sdk/commit/8a13ba922036d9ac77b862a70574aebf5d2673e7))
* promisify Context.releaseQueues() ([42ffc87](https://github.com/convertcom/javascript-sdk/commit/42ffc87f00770f1fd36ad1e5809fab18e4301d49))

## [4.1.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v4.0.1...js-sdk-v4.1.0) (2024-07-16)


### Features

* allow multiple transactions on tracking revenue goal ([4b33924](https://github.com/convertcom/javascript-sdk/commit/4b339245c83a02897629292bb3e98ab7cdac4b61))
* generate rollup config per package ([1f00878](https://github.com/convertcom/javascript-sdk/commit/1f008780cc716a697e1a80bb407159b783f88a9f))
* implement SDK Key/Secret with Authorization Bearer header ([19de29d](https://github.com/convertcom/javascript-sdk/commit/19de29d8961152ab26acdd51346b60248f664bf3))
* introduce BucketingError when deciding variations ([10caaae](https://github.com/convertcom/javascript-sdk/commit/10caaae7ba586a88b6e9d02acff43b3d6481c815))
* introduce Context.getVisitorData() ([5890e19](https://github.com/convertcom/javascript-sdk/commit/5890e19f76f83b209554fe792201d0f935e54659))
* return bucketing allocation upon deciding variation ([06fb25f](https://github.com/convertcom/javascript-sdk/commit/06fb25fb56477dfbd55e46af5d38dd53316cfdc9))


### Bug Fixes

* resolve missing dependencies for type declarations ([4fa0feb](https://github.com/convertcom/javascript-sdk/commit/4fa0feb2926acfc7ec82ec0b41c46b8f3753b7f1))
* update the shape of goalData for tracking revenue ([f54356f](https://github.com/convertcom/javascript-sdk/commit/f54356f2b76e2def36bf75d0f95c31134a2de74d))

## [4.0.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v4.0.0...js-sdk-v4.0.1) (2024-04-03)


### Bug Fixes

* include all visitor data in persistence dataStore if provided ([54c38d4](https://github.com/convertcom/javascript-sdk/commit/54c38d4c5546bb9369c591f66cac577a693f5099))

## [4.0.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v3.0.0...js-sdk-v4.0.0) (2024-03-23)


### ⚠ BREAKING CHANGES

* use generated API config types to match the published specs at https://api.convert.com/doc/serving/

### Features

* use generated API config types to match the published specs at https://api.convert.com/doc/serving/ ([681d038](https://github.com/convertcom/javascript-sdk/commit/681d03845c2d36e303930865275677e8a37faa15))

## [3.0.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v2.4.1...js-sdk-v3.0.0) (2024-03-12)


### ⚠ BREAKING CHANGES

* include experience key hash bucketing

### Features

* include experience key hash bucketing ([2b7df97](https://github.com/convertcom/javascript-sdk/commit/2b7df976506666b9ef251563008c18a5a00ed7ff))


### Bug Fixes

* more robust utility for checking plain object ([c4c28bf](https://github.com/convertcom/javascript-sdk/commit/c4c28bf82765054011a170bceeaa0488cf364437))

## [2.4.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v2.4.0...js-sdk-v2.4.1) (2024-02-23)


### Bug Fixes

* expose VariationChange, VariationChangeType, and FullStackFeatureChang ([d90aec0](https://github.com/convertcom/javascript-sdk/commit/d90aec09391c41630d72854d8416cd6a39ef77b7))

## [2.4.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v2.3.0...js-sdk-v2.4.0) (2024-02-23)


### Features

* introduce "Context.getConfigEntityById()" ([58ce097](https://github.com/convertcom/javascript-sdk/commit/58ce097f0bf048825d010a7ccc93225854311380))

## [2.3.0](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v2.2.4...js-sdk-v2.3.0) (2024-02-19)


### Features

* introduce Context.releaseQueues() that sends  pending API/DataStore queues to server ([d1ae104](https://github.com/convertcom/javascript-sdk/commit/d1ae1045aacb724aaac2512061c6cf1c96b461e7))

## [2.2.4](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v2.2.3...js-sdk-v2.2.4) (2024-02-19)


### Bug Fixes

* handle undefined default visitor properties ([4a5fd93](https://github.com/convertcom/javascript-sdk/commit/4a5fd93620ef38f65a969b9f950e964b0ee878e9))

## [2.2.3](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v2.2.2...js-sdk-v2.2.3) (2024-02-16)


### Bug Fixes

* ignore extended properties when firing events by the EventManager ([f623b9b](https://github.com/convertcom/javascript-sdk/commit/f623b9bdece54d50aa21da76b7c99ea33e632094))

## [2.2.2](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v2.2.1...js-sdk-v2.2.2) (2024-02-16)


### Bug Fixes

* case-insensitive "equal" and "matches" comparisons ([b9dea18](https://github.com/convertcom/javascript-sdk/commit/b9dea189fe0e4ff52ebfa2ec547205cfe9c30304))

## [2.2.1](https://github.com/convertcom/javascript-sdk/compare/js-sdk-v2.2.0...js-sdk-v2.2.1) (2024-02-15)


### Bug Fixes

* less/lessEqual comparison negation logic ([8a1ed8f](https://github.com/convertcom/javascript-sdk/commit/8a1ed8f9ddfb0bf89da1619e04c9b04b3c424480))

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
