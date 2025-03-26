# Convert Insights, Inc. JavaScript SDK

---

![coverage](https://convertcom.github.io/javascript-sdk/coverage.svg)
[![license](https://img.shields.io/badge/license-Apache--2.0-green)](https://choosealicense.com/licenses/apache-2.0/)

---

## Table of Contents

1. [About](#about)
2. [Contributing](#contributing)
3. [Documentation](#documentation)
4. [Requirements](#requirements)
   - [General](#1-general)
   - [Audiences](#2-audiences)
   - [Segments](#3-segments)
   - [Rules](#4-rules)
   - [Experiences](#5-experiences)
   - [Features](#6-features)
   - [Environments](#7-environments)
5. [Getting Started with Installation](#getting-started-with-installation)
6. [Using the SDK](#using-the-sdk)
   - [Comprehensive Examples](#comprehensive-examples)
   - [Import the SDK into Your Project](#import-the-sdk-into-your-project)
   - [Initialize Using the SDK Key](#initialize-using-the-sdk-key)
   - [Initialize Using Static Configuration](#initialize-using-static-configuration)
   - [SDK Configuration Options](#sdk-configuration-options)
   - [Create User Context](#create-user-context)
7. [SDK Methods for Running Experiences](#sdk-methods-for-running-experiences)
   - [Run All Active Experiences](#run-all-active-experiences)
   - [Run a Single Experience](#run-a-single-experience)
   - [Run Multiple Features](#run-multiple-features)
   - [Run a Single Feature](#run-a-single-feature)
   - [Track Conversion Events](#track-conversion-events)
   - [Run Custom Segments](#run-custom-segments)
   - [Set Default Segments](#set-default-segments)
   - [Update Visitor Properties](#update-visitor-properties)
   - [Get Config Entity](#get-config-entity)
   - [Get Config Entity by Id](#get-config-entity-by-id)
   - [Release Pending Queues](#release-pending-queues)
8. [Events](#events)
9. [Provide Persistent DataStore](#provide-persistent-datastore)
10. [Build Environment Variables](#build-environment-variables)
11. [Custom Build](#custom-build)
12. [Conclusion](#conclusion)
13. [Credits](#credits)

---

## About

This repository contains the **JavaScript SDK (Node.js and browser)** needed to integrate Convert.com's **Fullstack Product (feature tests, feature flags, feature rollouts)** into third-party apps. It is maintained by the Convert.com team.

---

## Contributing

See [CONTRIBUTING](./CONTRIBUTING.md) for details on how to submit your contributions.

---

## Documentation

Refer to the [JavaScript SDK Documentation](https://convertcom.github.io/javascript-sdk/) for details on the SDK modules and practical tutorials.

---

## Requirements

Fullstack projects differ slightly from web-based projects. Here are some key points:

### 1. General

All entities must have a unique identifier `key` that is a `string` when defined in the web interface.

### 2. Audiences

The SDK does not manage persistence directly. Instead, you can provide your own DataStore to make user bucketing persistent. All created audiences in a Fullstack project should be of type `transient`, where conditions are checked each time they encounter the experience.

### 3. Segments

Segments represent audiences of type `segmentation`. Conditions are checked on every page view until they are met. In a Fullstack project, segments do not stay persistent unless you use your own DataStore.

### 4. Rules

Rules are limited to the following types:

1. `generic_bool_key_value`: Matching type choices are limited to `equals`.
2. `generic_numeric_key_value`: Matching type choices are limited to `equalsNumber`, `less`, and `lessEqual`.
3. `generic_text_key_value`: Matching type choices are limited to `matches`, `regexMatches`, `contains`, `endsWith`, and `startsWith`.

> **Note:** Each rule must have a unique identifier `key` that is a `string`.

### 5. Experiences

Experiences are limited to the following types:

1. `a/b_fullstack`: An `a/b` experience.
2. `feature_rollout`: An experience with only one variation that is not the original.

### 6. Features

> Available only for Fullstack projects.

Features are variables that can be defined in the web interface and used by the SDK, supporting the following types:

1. `boolean`
2. `integer`
3. `string`
4. `json`

### 7. Environments

> Available only for Fullstack projects.

All matching rules apply only to entities within the same `environment`. You can create as many environments as you need. The default environments are:

1. `staging`
2. `production`

---

## Getting Started with Installation

Install the module using `npm` or `yarn`:

```bash
npm install --save @convertcom/js-sdk
```

The package's build contains definitions for generating entry points in different formats:

- CommonJS (`CJS`) - found under `lib/index.js` or `lib/index.min.js` for the minified version.
- ES Modules (`ESM`) - found under `lib/index.mjs` or `lib/index.min.mjs` for the minified version.
- Universal Module Definition (`UMD`) - found under `lib/index.umd.js` or `lib/index.umd.min.js` for the minified version.

You can also load the above entry points through [unpkg](https://unpkg.com/). This method adds a dependency on third-party URLs and is recommended only for evaluation purposes:

```html
<script src="https://unpkg.com/@convertcom/js-sdk/lib/index.js"></script>

<!-- or the minified version -->
<script src="https://unpkg.com/@convertcom/js-sdk/lib/index.min.js"></script>
```

---

## Using the SDK

### Comprehensive Examples

#### Simple NodeJS Demo Video

<iframe loading="lazy" title="Convert Fullstack Demo" src="https://player.vimeo.com/video/969106633?dnt=1&amp;app_id=122963" width="1200" height="675" frameborder="0" allow="autoplay; fullscreen; picture-in-picture; clipboard-write" data-rocket-lazyload="fitvidscompatible" data-lazy-src="https://player.vimeo.com/video/969106633?dnt=1&amp;app_id=122963" data-ll-status="loaded" class="entered lazyloaded"></iframe>

#### Detailed ReactJS based Tutorials

For more detailed tutorials and guides, please visit the following links:

- [Testing Conversion Events](https://convertcom.github.io/javascript-sdk/tutorial-test-conversion.html)
- [Testing Feature Flags](https://convertcom.github.io/javascript-sdk/tutorial-test-features.html)
- [Testing User Segments](https://convertcom.github.io/javascript-sdk/tutorial-test-segments.html)
- [Testing Variations](https://convertcom.github.io/javascript-sdk/tutorial-test-variations.html)

### Import the SDK into Your Project

**ES6:**

Supports `import` and `export` statements in modern frontend frameworks (e.g., React, Vue, Angular, etc.).

> Must be transpiled to work in client browsers.

```typescript
import ConvertSDK from '@convertcom/js-sdk';
```

**ESM:**

Supports `import` and `export` statements in ES Module packages (type `module` in `package.json`).

> Must be transpiled to work in client browsers.

```typescript
import ConvertSDKModule from '@convertcom/js-sdk';
const ConvertSDK = (ConvertSDKModule as any).default;
```

**CommonJS:**

Supports `require` and `exports` in a Node.js environment.

> Must be transpiled to work in client browsers.

```typescript
const {default: ConvertSDK} = require('@convertcom/js-sdk');
```

**UMD:**

> Works directly in client browsers.

```html
<script src="https://unpkg.com/@convertcom/js-sdk/lib/index.umd.min.js"></script>
```

```typescript
const {default: ConvertSDK} = window.ConvertSDK; // ConvertSDK is provided by https://unpkg.com/@convertcom/js-sdk/lib/index.umd.min.js
```

The SDK instance can be initialized by either providing an `sdkKey` or a full project's data object.

#### Initialize Using the SDK Key

Include `sdkKey` as a string property in the options object you pass to the constructor instance. The SDK will fetch the project configuration from Convert's CDN and will then refresh it every `dataRefreshInterval` milliseconds.

```typescript
import type {ConvertInterface, ConvertConfig} from '@convertcom/js-sdk';
import ConvertSDK from '@convertcom/js-sdk';

const convertSDK: ConvertInterface = new ConvertSDK({
  sdkKey: 'xxx',
  dataRefreshInterval: 300000, // in milliseconds (5 minutes)
  environment: 'staging' // can also be "live"; this is configured in the Convert UI.
} as ConvertConfig);
convertSDK.onReady().then(() => {
  // console.log('SDK Data:', convertSDK.data);
  // create user context
  // run experience(s)
  // ...
});
```

After this, the SDK has been successfully instantiated, the project config data has been downloaded, and it is ready to be used for starting a UserContext.

#### Initialize Using Static Configuration

Alternatively, instead of providing an `sdkKey`, you can provide a static project configuration. The project configuration data can be fetched from _https://cdn-4.convertexperiments.com/api/v1/config/account_id/project_id_.

```typescript
import type {ConvertInterface, ConvertConfig} from '@convertcom/js-sdk';
import ConvertSDK from '@convertcom/js-sdk';

const projectConfig = {
  // your static project configuration data
};

const convertSDK: ConvertInterface = new ConvertSDK({
  projectConfig,
  environment: 'staging' // can also be "live"; this is configured in the Convert UI.
} as ConvertConfig);
convertSDK.onReady().then(() => {
  // create user context
  // run experience(s)
});
```

When using static project data, the SDK is instantiated as soon as the instance is created and can be used right away for starting a UserContext.

### SDK Configuration Options

The following shows the object model for the configuration options:

```typescript
import {LogLevel} from '@convertcom/js-sdk';

const config = {
  sdkKey: '', // either this or 'data' has to be provided
  environment: 'staging',
  logger: {
    logLevel: LogLevel.DEBUG,
    customLoggers: [] // Allows 3rd party loggers to be passed
  },
  bucketing: {
    hash_seed: 9999, // murmurhash seed
    max_traffic: 10000, // max hash (representing 100% traffic allocation)
    excludeExperienceIdHash: false // whether to ignore prefixing the generated hash with the experience id
  },
  dataStore: null, // Allows 3rd party data store to be passed (optional)
  dataRefreshInterval: 300000, // in milliseconds (5 minutes)
  data: projectData,
  events: {
    batch_size: 10, // max network requests to be released per call
    release_interval: 10000 // time in milliseconds between network releasing queue (including the initial release)
  },
  network: {
    tracking: true, // can be set to false to disable tracking events
    cacheLevel: 'default', // can be set to 'low' for short-lived cache (for development purposes only)
    source: 'js-sdk' // string identifier indicating the source of network requests
  }
};
```

The `projectData` structure is described at [https://api.convert.com/doc/serving/#tag/Project-Config](https://api.convert.com/doc/serving/#tag/Project-Config).

### Create User Context

Once the SDK has been created and instantiated, the next step is to create a unique context for the user in scope.

When creating the UserContext, a unique `userId` is required. This will be used to decide which variation/feature to present. As long as this `userId` does not change for a user and the experience configuration remains the same (variations not getting added or removed and traffic allocation not changed), the feature/variation bucketing will stay unchanged. Otherwise, see [Provide Persistent DataStore](#provide-persistent-datastore).

For convenience, a list of **User Properties** that can be later used inside audience definition evaluation can be provided when creating a UserContext. Any of these can be changed by providing them again when calling any of the functional SDK methods for running experiences, under the `attributes.visitorProperties` while setting the value for `attributes.updateVisitorProperties` to `true`.

```typescript
import type {ContextInterface} from '@convertcom/js-sdk';

const userContext: ContextInterface = convertSDK.createContext(
  'user-unique-id',
  {
    country: 'US',
    language: 'en'
  }
);
```

After creating a `userContext`, methods for running experiences can be called:

```typescript
import type {BucketedVariation} from '@convertcom/js-sdk';

const variation: BucketedVariation =
  userContext.runExperience('experience-key');
```

---

## SDK Methods for Running Experiences

### General Considerations

Every method that `userContext` exposes to run one or more experiences can receive an object with two keys:

- `locationProperties` - key-value object containing properties used for evaluating experiences' locations.
- `visitorProperties` - key-value object containing properties used for evaluating experiences' audiences, which will overwrite the same keys provided when creating the `userContext`.

Additionally, the `environment` can be provided as a third parameter.

### Run All Active Experiences

This method loops through each of the active experiences, runs them, and returns the selected variation for each experience. It decides whether the user should be bucketed into all variations based on the configuration rules (locations and audiences).

#### Parameters

| Parameter  | Type   | Required | Description                                                                                                              |
| ---------- | ------ | -------- | ------------------------------------------------------------------------------------------------------------------------ |
| attributes | object | No       | An object specifying attributes for the user.                                                                            |
|            |        |          | `locationProperties`: An object of key-value pairs used for location matching.                                           |
|            |        |          | `visitorProperties`: An object of key-value pairs used for audience targeting.                                           |
|            |        |          | `updateVisitorProperties`: Optional boolean for updating in-memory visitor properties.                                   |
|            |        |          | `enableTracking`: Optional boolean for controlling whether to track bucketing events immediately (_Defaults to `true`_). |
|            |        |          | `environment`: Optional string.                                                                                          |

#### Returns

List of bucketed variations.

#### Example

```typescript
import type {
  ConvertInterface,
  ConvertConfig,
  ContextInterface,
  BucketedVariation
} from '@convertcom/js-sdk';
import ConvertSDK from '@convertcom/js-sdk';

const config: ConvertConfig = {
  // full configuration options
};

const convertSDK: ConvertInterface = new ConvertSDK(config);
convertSDK.onReady().then(() => {
  const context: ContextInterface = convertSDK.createContext('user-unique-id');
  const variations: BucketedVariation[] = context.runExperiences();
  console.log(variations);
});
```

### Run a Single Experience

Decides whether the user should be bucketed into a single variation mapped to an experience's unique key. The decision is made based on the configured experience rules.

#### Parameters

| Parameter     | Type   | Required | Description                                                                                                              |
| ------------- | ------ | -------- | ------------------------------------------------------------------------------------------------------------------------ |
| experienceKey | string | Yes      | An experience's key that should be activated.                                                                            |
| attributes    | object | No       | An object specifying attributes for the user.                                                                            |
|               |        |          | `locationProperties`: An object of key-value pairs used for location matching.                                           |
|               |        |          | `visitorProperties`: An object of key-value pairs used for audience targeting.                                           |
|               |        |          | `updateVisitorProperties`: Optional boolean for updating in-memory visitor properties.                                   |
|               |        |          | `enableTracking`: Optional boolean for controlling whether to track bucketing events immediately (_Defaults to `true`_). |
|               |        |          | `environment`: Optional string.                                                                                          |

#### Returns

Bucketed variation.

#### Example

```typescript
import type {
  ConvertInterface,
  ConvertConfig,
  ContextInterface,
  BucketedVariation
} from '@convertcom/js-sdk';
import ConvertSDK from '@convertcom/js-sdk';

const config: ConvertConfig = {
  // full configuration options
};

const convertSDK: ConvertInterface = new ConvertSDK(config);
convertSDK.onReady().then(() => {
  const context: ContextInterface = convertSDK.createContext('user-unique-id');
  const variation: BucketedVariation = context.runExperience('experience-key');
  console.log(variation);
});
```

### Run Multiple Features

Retrieves a list of features that the user is already bucketed into.

#### Parameters

| Parameter  | Type   | Required | Description                                                                                                              |
| ---------- | ------ | -------- | ------------------------------------------------------------------------------------------------------------------------ |
| attributes | object | No       | An object specifying attributes for the user.                                                                            |
|            |        |          | `locationProperties`: An object of key-value pairs used for location matching.                                           |
|            |        |          | `visitorProperties`: An object of key-value pairs used for audience targeting.                                           |
|            |        |          | `updateVisitorProperties`: Optional boolean for updating in-memory visitor properties.                                   |
|            |        |          | `environment`: Optional string.                                                                                          |
|            |        |          | `typeCasting`: Optional boolean to control automatic type conversion to the variable's defined type. Defaults to `true`. |

#### Returns

List of bucketed features.

#### Example

```typescript
import type {
  ConvertInterface,
  ConvertConfig,
  ContextInterface,
  BucketedFeature
} from '@convertcom/js-sdk';
import ConvertSDK from '@convertcom/js-sdk';

const config: ConvertConfig = {
  // full configuration options
};

const convertSDK: ConvertInterface = new ConvertSDK(config);
convertSDK.onReady().then(() => {
  const context: ContextInterface = convertSDK.createContext('user-unique-id');
  const features: BucketedFeature[] = context.runFeatures();
  console.log(features);
});
```

### Run a Single Feature

Retrieves a single feature that the user is already bucketed into, mapped to a feature's unique key.

#### Parameters

| Parameter  | Type   | Required | Description                                                                                                              |
| ---------- | ------ | -------- | ------------------------------------------------------------------------------------------------------------------------ |
| featureKey | string | Yes      | A feature key.                                                                                                           |
| attributes | object | No       | An object specifying attributes for the user.                                                                            |
|            |        |          | `locationProperties`: An object of key-value pairs used for location matching.                                           |
|            |        |          | `visitorProperties`: An object of key-value pairs used for audience targeting.                                           |
|            |        |          | `updateVisitorProperties`: Optional boolean for updating in-memory visitor properties.                                   |
|            |        |          | `environment`: Optional string.                                                                                          |
|            |        |          | `typeCasting`: Optional boolean to control automatic type conversion to the variable's defined type. Defaults to `true`. |
|            |        |          | `experienceKeys`: Optional array of strings to use only specified experiences.                                           |

#### Returns

Bucketed feature.

#### Example

```typescript
import type {
  ConvertInterface,
  ConvertConfig,
  ContextInterface,
  BucketedFeature
} from '@convertcom/js-sdk';
import ConvertSDK from '@convertcom/js-sdk';

const config: ConvertConfig = {
  // full configuration options
};

const convertSDK: ConvertInterface = new ConvertSDK(config);
convertSDK.onReady().then(() => {
  const context: ContextInterface = convertSDK.createContext('user-unique-id');
  const feature: BucketedFeature = context.runFeature('feature-key');
  console.log(feature);
});
```

### Track Conversion Events

Decides whether to send a conversion event, mapped to a goal's unique key. The decision is made against the configured goal-triggering rules.

#### Parameters

| Parameter  | Type   | Required | Description                                                                                                      |
| ---------- | ------ | -------- | ---------------------------------------------------------------------------------------------------------------- |
| goalKey    | string | Yes      | A goal key                                                                                                       |
| attributes | object | No       | An object that specifies attributes for the user. Accepts 2 properties:                                          |
|            |        |          | `ruleData` an object of key-value pairs that are used for goal matching.                                         |
|            |        |          | `conversionData` an array of key-value pairs that are used for transaction data. Each pair accepts 3 properties: |
|            |        |          | 1. `amount` a number represents the order value.                                                                 |
|            |        |          | 2. `productsCount` a number represents the order quantity.                                                       |
|            |        |          | 3. `transactionId` a unique number or string represents the paid transaction.                                    |
|            |        |          | `conversionSetting` an object of key-value pairs that are used for tracking settings:                            |
|            |        |          | 1. `forceMultipleTransactions` a boolean decides whether to accumulate revenue for the same visitor.             |

#### Returns

Void.

#### Example

```typescript
import type {
  ConvertInterface,
  ConvertConfig,
  ContextInterface
} from '@convertcom/js-sdk';
import ConvertSDK from '@convertcom/js-sdk';

const config: ConvertConfig = {
  // full configuration options
};

const convertSDK: ConvertInterface = new ConvertSDK(config);
convertSDK.onReady().then(() => {
  const context: ContextInterface = convertSDK.createContext('user-unique-id');
  context.trackConversion('goal-key', {
    ruleData: {
      action: 'buy'
    },
    conversionData: [
      {
        key: 'amount',
        value: 10.3
      },
      {
        key: 'productsCount',
        value: 2
      },
      {
        key: 'transactionId',
        value: 'transaction-unique-id'
      }
    ],
    conversionSetting: {
      forceMultipleTransactions: false
    }
  });
});
```

### Run Custom Segments

Decides whether to update custom segments in the user context, mapped to segments' unique keys. The decision is made against the configured segment rules.

#### Parameters

| Parameter         | Type   | Required | Description                                              |
| ----------------- | ------ | -------- | -------------------------------------------------------- |
| segmentsKeys      | array  | Yes      | A list of segment keys.                                  |
| visitorProperties | object | No       | An object of key-value pairs used for segments matching. |

#### Returns

Void.

#### Example

```typescript
import type {
  ConvertInterface,
  ConvertConfig,
  ContextInterface
} from '@convertcom/js-sdk';
import ConvertSDK from '@convertcom/js-sdk';

const config: ConvertConfig = {
  // full configuration options
};

const convertSDK: ConvertInterface = new ConvertSDK(config);
convertSDK.onReady().then(() => {
  const context: ContextInterface = convertSDK.createContext('user-unique-id');
  context.runCustomSegments(['segment-key'], {
    enabled: true
  });
});
```

### Set Default Segments

Permanently update the visitor segments for reporting purposes. Only the following properties are included in Convert Reports: `browser`, `devices`, `source`, `campaign`, `visitorType`, and `country`.

#### Parameters

| Parameter | Type   | Required | Description                                                                                                                  |
| --------- | ------ | -------- | ---------------------------------------------------------------------------------------------------------------------------- |
| segments  | object | Yes      | An object of key-value pairs to be merged with the initial **User Properties** created with [context](#create-user-context). |

#### Returns

Void.

#### Example

```typescript
import type {
  ConvertInterface,
  ConvertConfig,
  ContextInterface
} from '@convertcom/js-sdk';
import ConvertSDK from '@convertcom/js-sdk';

const config: ConvertConfig = {
  // full configuration options
};

const convertSDK: ConvertInterface = new ConvertSDK(config);
convertSDK.onReady().then(() => {
  const context: ContextInterface = convertSDK.createContext('user-unique-id');
  context.setDefaultSegments({
    country: 'US'
  });
});
```

### Update Visitor Properties

Permanently update all of the visitor properties used inside audience definition evaluation.

#### Parameters

| Parameter         | Type   | Required | Description                                                                                                                  |
| ----------------- | ------ | -------- | ---------------------------------------------------------------------------------------------------------------------------- |
| visitorId         | string | Yes      | User unique ID.                                                                                                              |
| visitorProperties | object | Yes      | An object of key-value pairs to be merged with the initial **User Properties** created with [context](#create-user-context). |

#### Returns

Void.

#### Example

```typescript
import type {
  ConvertInterface,
  ConvertConfig,
  ContextInterface
} from '@convertcom/js-sdk';
import ConvertSDK from '@convertcom/js-sdk';

const config: ConvertConfig = {
  // full configuration options
};

const convertSDK: ConvertInterface = new ConvertSDK(config);
convertSDK.onReady().then(() => {
  const context: ContextInterface = convertSDK.createContext('user-unique-id');
  context.updateVisitorProperties({
    weather: 'rainy'
  });
});
```

### Get Config Entity

Find a single entity in configuration by `key`.

#### Parameters

| Parameter  | Type       | Required | Description                                                                                                                                                                                          |
| ---------- | ---------- | -------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| key        | string     | Yes      | Entity key as found in configuration.                                                                                                                                                                |
| entityType | EntityType | Yes      | One of the configuration entities: `EntityType.AUDIENCE`, `EntityType.LOCATION`, `EntityType.SEGMENT`, `EntityType.FEATURE`, `EntityType.GOAL`, `EntityType.EXPERIENCE`, and `EntityType.VARIATION`. |

#### Returns

Void.

#### Example

```typescript
import type {
  ConvertInterface,
  ConvertConfig,
  ContextInterface,
  Experience
} from '@convertcom/js-sdk';
import ConvertSDK, {EntityType} from '@convertcom/js-sdk';

const config: ConvertConfig = {
  // full configuration options
};

const convertSDK: ConvertInterface = new ConvertSDK(config);
convertSDK.onReady().then(() => {
  const context: ContextInterface = convertSDK.createContext('user-unique-id');
  const exprience: Experience = context.getConfigEntity(
    'experience-key',
    EntityType.EXPERIENCE
  );
});
```

### Get Config Entity by Id

Find a single entity in configuration by `id`.

#### Parameters

| Parameter  | Type       | Required | Description                                                                                                                                                                                          |
| ---------- | ---------- | -------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| id         | number     | Yes      | Entity id as found in configuration.                                                                                                                                                                 |
| entityType | EntityType | Yes      | One of the configuration entities: `EntityType.AUDIENCE`, `EntityType.LOCATION`, `EntityType.SEGMENT`, `EntityType.FEATURE`, `EntityType.GOAL`, `EntityType.EXPERIENCE`, and `EntityType.VARIATION`. |

#### Returns

Void.

#### Example

```typescript
import type {
  ConvertInterface,
  ConvertConfig,
  ContextInterface,
  BucketedVariation,
  Feature,
  VariationChange
} from '@convertcom/js-sdk';
import ConvertSDK, {EntityType, VariationChangeType} from '@convertcom/js-sdk';

const config: ConvertConfig = {
  // full configuration options
};

const convertSDK: ConvertInterface = new ConvertSDK(config);
convertSDK.onReady().then(() => {
  const context: ContextInterface = convertSDK.createContext('user-unique-id');
  const variation: BucketedVariation = context.runExperience('experience-key');
  const changesData: VariationChange = variation.changes.find(
    ({type}) => type === VariationChangeType.FULLSTACK_FEATURE
  );
  const feature: Feature = context.getConfigEntityById(
    changesData.data.feature_id,
    EntityType.FEATURE
  );
});
```

### Release Pending Queues

Send all pending API/DataStore queues to the server.

#### Parameters

| Parameter | Type   | Required | Description                            |
| --------- | ------ | -------- | -------------------------------------- |
| reason    | string | No       | Custom message for debugging purposes. |

#### Returns

Void.

#### Example

```typescript
import type {
  ConvertInterface,
  ConvertConfig,
  ContextInterface,
  BucketedVariation
} from '@convertcom/js-sdk';
import ConvertSDK from '@convertcom/js-sdk';

const config: ConvertConfig = {
  // full configuration options
};

const convertSDK: ConvertInterface = new ConvertSDK(config);
convertSDK.onReady().then(() => {
  const context: ContextInterface = convertSDK.createContext('user-unique-id');
  const variations: BucketedVariation[] = context.runExperiences();

  // manually release all pending queue at some point later, like on click, component unmount
  context.releaseQueues().then(() => {
    console.log('all pending queue has been released');
  });
});
```

### Events

You can capture SDK events as well:

| Event                  | Triggered by                                           | Callback data                                                                            |
| ---------------------- | ------------------------------------------------------ | ---------------------------------------------------------------------------------------- |
| `ready`                | Initializing the SDK                                   | null                                                                                     |
| `bucketing`            | Running experience(s)                                  | { visitorId: `string`, experienceKey: `string`, variationKey: `string` }                 |
|                        | Running feature(s)                                     | { visitorId: `string`, experienceKey: `string`, featureKey: `string`, status: `string` } |
| `conversion`           | Tracking conversion                                    | { visitorId: `string`, goalKey: `string` }                                               |
| `location.activated`   | Location rules matched                                 | { visitorId: `string`, location: { id: `string`, name: `string`, key: `string` } }       |
| `location.deactivated` | Location rules not matched (only if activated earlier) | { visitorId: `string`, location: { id: `string`, name: `string`, key: `string` } }       |
| `config.updated`       | Refreshing the configuration                           | null                                                                                     |

```typescript
import type {
  ConvertInterface,
  ConvertConfig,
  ContextInterface,
  Experience,
  Variation
} from '@convertcom/js-sdk';
import ConvertSDK, {EntityType, SystemEvents} from '@convertcom/js-sdk';

const convertSDK: ConvertInterface = new ConvertSDK({
  sdkKey: 'xxx'
} as ConvertConfig);

convertSDK.on(SystemEvents.READY, function (res, err) {
  if (err) {
    console.error(err);
  }
});

convertSDK.on(
  SystemEvents.BUCKETING,
  function ({visitorId, experienceKey, variationKey, featureKey, status}, err) {
    if (err) {
      console.error(err);
    } else {
      console.log(visitorId, experienceKey, variationKey, featureKey, status);
      // Example GA integrations can be done here
      // Note that you need to create audiences manually on the GA side
      const experienceName: Experience = context.getConfigEntity(
        experienceKey,
        EntityType.EXPERIENCE
      ).name;
      const variationName: Variation = context.getConfigEntity(
        variationKey,
        EntityType.VARIATION
      ).name;
      gtag('event', 'YOUR_GA_CUSTOM_EVENT', {
        experienceName,
        variationName
      });
    }
  }
);

convertSDK.on(SystemEvents.CONVERSION, function ({visitorId, goalKey}, err) {
  if (err) {
    console.error(err);
  } else {
    console.log(visitorId, goalKey);
  }
});

convertSDK.on(SystemEvents.CONFIG_UPDATED, function (res, err) {
  if (err) {
    console.error(err);
  }
});
```

### Provide Persistent DataStore

> **Note:** This feature is optional.

You can provide your own DataStore to make user bucketing persistent, ensuring consistent experience variation selection regardless of changes in experience configurations. As long as experience variations and traffic allocation do not change, the initial user bucketing remains persistent. The [MurmurHash algorithm](https://en.wikipedia.org/wiki/MurmurHash) is used to decide which variation will be selected, ensuring consistency.

The provided DataStore interface is expected to have two methods: `set` and `get`.

```typescript
import type {
  ConvertInterface,
  ConvertConfig,
  ContextInterface
} from '@convertcom/js-sdk';
import ConvertSDK from '@convertcom/js-sdk';

const config: ConvertConfig = {
  // full configuration options
};

class CustomDataStore {
  #data = {};

  get(key) {
    if (!key) return this.#data;
    return this.#data[key.toString()];
  }

  set(key, value) {
    if (!key) throw new Error('Invalid CustomDataStore key!');
    this.#data[key.toString()] = value;
  }
}

const dataStore = new CustomDataStore();
const convertSDK: ConvertInterface = new ConvertSDK({
  ...config,
  dataStore
} as ConvertConfig);
```

### Common Errors and Troubleshooting

#### Error: SDK Initialization Failed

- **Cause:** Incorrect `sdkKey` or network issues.
- **Solution:** Verify the `sdkKey` and ensure network connectivity.

#### Error: User Context Creation Failed

- **Cause:** Missing or invalid `userId`.
- **Solution:** Ensure a valid `userId` is provided during context creation.

#### Error: Experience Not Found

- **Cause:** Incorrect `experienceKey` or experience not defined in the project.
- **Solution:** Verify the `experienceKey` and ensure the experience is correctly configured in the project.

#### Error: Feature Not Found

- **Cause:** Incorrect `featureKey` or feature not defined in the project.
- **Solution:** Verify the `featureKey` and ensure the feature is correctly configured in the project.

---

## Build Environment Variables

| Environment Variable | Description                                                                                    | Value                                                                     |
| -------------------- | ---------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------- |
| `LOG_LEVEL`          | Specifies the level of log statements to keep, while removing the rest from the bundle output. | `0` = ALL, `1` = DEBUG, `2` = INFO, `3` = WARN, `4` = ERROR, `5` = SILENT |
| `BUNDLES`            | Comma-separated tokens for specifying which bundles to build. Defaults to include all bundles. | `cjs`, `cjs-legacy`, `esm`, `umd`                                         |

---

## Custom Build

You can use a customized build in certain situations. For example:

1. To reduce bundle size and remove all log statements: `LOG_LEVEL=5 yarn sdk:build`
2. To build CommonJS bundles only: `BUNDLES=cjs,cjs-legacy yarn sdk:build`

Additionally, you can even include this repository as part of your own `TypeScript` project:

1. Add the Convert JavaScript SDK repository to your own as a submodule:

   ```bash
   git submodule add https://github.com/convertcom/javascript-sdk javascript-sdk
   ```

2. Choose one of the following methods:

   - **Workspaces**: Add the following `workspaces` to your `package.json` (_assuming that the submodule is located under your repository root_):

     ```json
     {
       "workspaces": ["javascript-sdk/packages/*"]
     }
     ```

     If using PnP, add the following `packages` to your `pnpm-workspace.yaml` instead:

     ```yaml
     packages:
       - 'javascript-sdk/packages/*'
     ```

   - **Compiler Options**: Add the following `paths` under `compilerOptions` of your `tsconfig.json` (for more information, refer to the [TypeScript documentation](https://www.typescriptlang.org/docs/handbook/tsconfig-json.html)):

     ```json
     {
       "compilerOptions": {
         "paths": {
           "@convertcom/js-sdk-api": ["./javascript-sdk/packages/api"],
           "@convertcom/js-sdk-bucketing": [
             "./javascript-sdk/packages/bucketing"
           ],
           "@convertcom/js-sdk-data": ["./javascript-sdk/packages/data"],
           "@convertcom/js-sdk-enums": ["./javascript-sdk/packages/enums"],
           "@convertcom/js-sdk-event": ["./javascript-sdk/packages/event"],
           "@convertcom/js-sdk-experience": [
             "./javascript-sdk/packages/experience"
           ],
           "@convertcom/js-sdk-logger": ["./javascript-sdk/packages/logger"],
           "@convertcom/js-sdk-rules": ["./javascript-sdk/packages/rules"],
           "@convertcom/js-sdk-segments": [
             "./javascript-sdk/packages/segments"
           ],
           "@convertcom/js-sdk-types": ["./javascript-sdk/packages/types"],
           "@convertcom/js-sdk-utils": ["./javascript-sdk/packages/utils"],
           "@convertcom/js-sdk": ["./javascript-sdk/packages/js-sdk"]
         }
       }
     }
     ```

3. Add the following script to your `package.json`:

   ```json
   {
     "scripts": {
       "build:sdk": "cd javascript-sdk && BUNDLES=cjs,esm LOG_LEVEL=5 yarn sdk:build"
     }
   }
   ```

   > Note that both `BUNDLES` and `LOG_LEVEL` are optional (_see [Build Environment Variables](#build-environment-variables) above_)

4. Now you can build the Convert JavaScript SDK alongside your project (_assuming that you use `rollup` for bundling_):

   ```json
   {
     "scripts": {
       "build": "yarn build:sdk && rollup -c"
     }
   }
   ```

5. You need to run `yarn` inside the submodule `javascript-sdk` as well:

   ```bash
   # Update ConvertSDK submodule
   git submodule update --init --remote
   # Install your project dependencies as per your `package.json`
   yarn
   # Install ConvertSDK dependencies
   cd javascript-sdk && yarn
   # Build your project
   yarn build
   ```

---

## Conclusion

This document provides a comprehensive guide on how to integrate and use the Convert Insights, Inc. JavaScript SDK in your projects. It covers all necessary steps from installation and configuration to advanced usage and troubleshooting.

If you encounter any issues or have questions, feel free to reach out to the Convert.com support team or open an issue on the [GitHub repository](https://github.com/convertcom/javascript-sdk).

Happy converting!

---

## Credits

Copyright © 2024 Convert Insights, Inc.
