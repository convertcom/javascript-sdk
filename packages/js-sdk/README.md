# Convert Insights, Inc. JavaScript SDK

![coverage](https://convertcom.github.io/javascript-sdk/coverage.svg)
[![license](https://img.shields.io/badge/license-Apache--2.0-green)](https://choosealicense.com/licenses/apache-2.0/)

## About

This repository contains the **JavaScript SDK (nodeJs and browser)** needed to integrate Convert.com's **Fullstack Product (feature tests, feature flags, feature rollouts)** inside third-party apps. This is maintained by the Convert.com team.

## Contributing

See [CONTRIBUTING](./CONTRIBUTING.md) for details on how to submit your contribution.

## Documentation

See [JavaScript SDK Documentation](https://convertcom.github.io/javascript-sdk/) for details on the SDK modules, in addition to practical tutorials.

## Requirements

Fullstack projects are slightly different compared to web-based projects, so that

### 1. General

All entities must have a unique identifier `key` that is a `string` when defined at the web interface.

### 2. Audiences

There is no persistence managed by the SDK directly, instead, as a developer you can provide your own DataStore that is used to make user bucketing persistent. That said, all created Audiences in a Fullstack project shall be of type `transient` by definition, where conditions will be checked each time they encounter the experience.

### 3. Segments

Represent Audiences of type `segmentation`, conditions will be checked on every page-view until they are met. The only difference in a Fullstack project is that if met, the segment does not stay persistent unless you use your own DataStore.

### 4. Rules

Limited to the following types:

1. `generic_bool_key_value`: matching type choices are limited to `equals`
2. `generic_numeric_key_value`: matching type choices are limited to `equalsNumber`, `less`, and `lessEqual`
3. `generic_text_key_value`: matching type choices are limited to `matches`, `regexMatches`, `contains`, `endsWith`, and `startsWith`

> **Note:** Each rule must have a unique identifier `key` that is a `string`

### 5. Experiences

Limited to the following types:

1. `a/b_fullstack`: an `a/b` experience
2. `feature_rollout`: an experience with only one variation that is not the original

### 6. Features

> Available only to Fullstack projects.

Variables that can be defined at the web interface and used by the SDK, supporting the following types:

1. `boolean`
2. `integer`
3. `string`
4. `json`

### 7. Environments

> Available only to Fullstack projects.

All matching rules apply only to entities having the same `environment`. You can create as many environments as you wish, the defaults are:

1. `staging`
2. `production`

## Getting Started

Install module using `npm` or `yarn`:

```bash
npm install --save @convertcom/js-sdk
```

Package's build contains definitions for generating entry points in different formats:

- CommonJS `CJS` - can be found under `lib/index.js`or`lib/index.min.js` for the minified version;
- ES Moduls `ESM` - can be found under `lib/index.mjs`or`lib/index.min.mjs` for the minified version;
- Universal Module Definition `UMD` - can be found under `lib/index.umd.js` or `lib/index.umd.min.js` for the minified version;

The above entry points can also be loaded through [unpkg](https://unpkg.com/) but this method will add a dependency on third-party URLs and therefore is recommended to use it only for evaluation purposes:

```html
<script src="https://unpkg.com/@convertcom/js-sdk/lib/index.js"></script>

<!-- or the minified version -->
<script src="https://unpkg.com/@convertcom/js-sdk/lib/index.min.js"></script>
```

## SDK Usage

### Import the SDK Inside Your Project

**ES6:**

Supports `import` and `export` statements in modern JS (_i.e. React, Vue, Angular, .. etc_).

> Must be transpiled to work in client browsers.

```typescript
import ConvertSDK from '@convertcom/js-sdk';
```

**CommonJS:**

Supports `require` and `exports` in NodeJS environment.

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

The SDK instance can be initialized by either providing an SDK key or a full project's data object.

#### Initialize using the SDK Key

Include `sdkKey` as a string property in the options object you pass to the constructor instance. The SDK will fetch the project configuration from Convert's CDN and will then refresh it every `dataRefreshInterval` seconds.

```typescript
import ConvertSDK, {ConvertInterface, ConvertConfig} from '@convertcom/js-sdk';

const convertSDK: ConvertInterface = new ConvertSDK({
  sdkKey: 'xxx',
  dataRefreshInterval: 300000 // in milliseconds (5 minutes)
} as ConvertConfig);
convertSDK.onReady().then(() => {
  // create user context
  // run experience(s)
  // ...
});
```

After this point, the SDK has been successfully instantiated, project config data has been downloaded, and it is ready to be used for starting a UserContext.

#### Initialize using Static Configuration

Alternatively, to provide an **SDK** key\*\*, a static project configuration can be given. The project configuration data can be fetched from https://cdn-4.convertexperiments.com/api/v1/config/account_id/project_id

```typescript
import {ConvertInterface, ConvertConfig} from '@convertcom/js-sdk';

const config: ConvertConfig = {
  data: projectData // full project's data
};

const convertSDK: ConvertInterface = new ConvertSDK(config);
```

When using static project data, the SDK is instantiated as soon as the instance is created and can be used right away for starting a UserContext.

### SDK Configuration Options

The following shows the object model for the configuration options:

```typescript
import {LogLevel} from '@convertcom/js-sdk';

const config = {
  sdkKey: '' // either this or 'data' has to be provided
  environment: 'staging',
  logger: {
    logLevel: LogLevel.DEBUG,
    customLoggers: [] // Allows 3rd party loggers to be passed
  },
  dataStore: null, // Allows 3rd party data store to be passed (optional)
  dataRefreshInterval: 300000, // in milliseconds (5 minutes)
  data: projectData,
  network: {
    tracking: true, // can be set to false to disable tracking events
    cacheLevel: 'default' // can be set to 'low' for short-lived cache (for development purposes only)
  }
};
```

`projectData` structure is as described at [https://api.convert.com/doc/serving/#tag/Project-Config](https://api.convert.com/doc/serving/#tag/Project-Config)

### Create User Context

Once the SDK has been created and instantiated, the next step is to create a unique context for the user in scope.

When creating the UserContext, a unique `userId` is required to be provided. This will be used in the process of deciding which variation/feature to be presented. As long as this `userId` does not change for a user and the experience configuration remains the same(variations not getting added or removed and traffic allocation not changed), the feature/variation bucketing will stay unchanged. Otherwise, see [Persistent Datastore](#provide-persistent-datastore).

For convenience, a list of **User Properties** that can be later used inside Audience definition evaluation can be provided when creating a UserContext. Any of these can be changed by providing them again when calling any of the functional SDK methods for running experiences, under the `attributes.visitorProperties` while setting the value for `attributes.updateVisitorProperties` to `true`

```typescript
import {ContextInterface} from '@convertcom/js-sdk';

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
import {BucketedVariation} from '@convertcom/js-sdk';

const variation: BucketedVariation =
  userContext.runExperience('experience-key');
```

## SDK Methods for Running Experiences

### General Considerations

Every method that `userContext` exposes to run one or more experiences, can receive as a parameter an object with two keys:

- `locationProperties` - key: value object containing properties that are used when evaluating Experiences' Locations;
- `visitorProperties` - key: value object containing properties that are used when evaluating Experiences' Audiences and which will overwrite the same keys provided when creating the `userContext`;

On top of this, the `environment` can be provided as a third parameter.

### Run All Active Experiences

The method will loop through each of the active experiences, run them and return, for each of the experiences, the selected variation. Decides whether the user should be bucketed into all variations based on the configuration rules (_`locations` and `audiences`_).

#### Parameters

| Parameter  | Type   | Required | Description                                                                            |
| ---------- | ------ | -------- | -------------------------------------------------------------------------------------- |
| attributes | object | No       | An object that specifies attributes for the user. Accepts 3 properties:                |
|            |        |          | `locationProperties` an object of key-value pairs that are used for location matching. |
|            |        |          | `visitorProperties` an object of key-value pairs that are used for audience targeting. |
|            |        |          | `updateVisitorProperties` optional boolean for updating in-memory visitor properties.  |
|            |        |          | `environment` optional string.                                                         |

#### Returns

List of bucketed variations.

#### Example

```typescript
import ConvertSDK, {
  ConvertInterface,
  ConvertConfig,
  ContextInterface,
  BucketedVariation
} from '@convertcom/js-sdk';

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

Decides whether the user should be bucketed into a single variation, which is mapped to an experience unique key. The decision is made against the configured experience rules.

#### Parameters

| Parameter     | Type   | Required | Description                                                                            |
| ------------- | ------ | -------- | -------------------------------------------------------------------------------------- |
| experienceKey | string | Yes      | An experience's key that should be activated                                           |
| attributes    | object | No       | An object that specifies attributes for the user. Accepts 3 properties:                |
|               |        |          | `locationProperties` an object of key-value pairs that are used for location matching. |
|               |        |          | `visitorProperties` an object of key-value pairs that are used for audience targeting. |
|               |        |          | `updateVisitorProperties` optional boolean for updating in-memory visitor properties.  |
|               |        |          | `environment` optional string.                                                         |

#### Returns

Bucketed variation.

#### Example

```typescript
import ConvertSDK, {
  ConvertInterface,
  ConvertConfig,
  ContextInterface,
  BucketedVariation
} from '@convertcom/js-sdk';

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

Retrieves a list of features that the user already bucketed.

#### Parameters

| Parameter  | Type   | Required | Description                                                                                                                                                 |
| ---------- | ------ | -------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| attributes | object | No       | An object that specifies attributes for the user. Accepts 4 properties:                                                                                     |
|            |        |          | `locationProperties` an object of key-value pairs that are used for location matching.                                                                      |
|            |        |          | `visitorProperties` an object of key-value pairs that are used for audience targeting.                                                                      |
|            |        |          | `updateVisitorProperties` optional boolean for updating in-memory visitor properties.                                                                       |
|            |        |          | `environment` optional string.                                                                                                                              |
|            |        |          | `typeCasting` optional boolean to dontrol automatic type conversion to the variable's defined type. Does not do any JSON validation (_defaults to `true`_). |

#### Returns

List of bucketed features.

#### Example

```typescript
import ConvertSDK, {
  ConvertInterface,
  ConvertConfig,
  ContextInterface,
  BucketedFeature
} from '@convertcom/js-sdk';

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

Retrieves a single feature that the user already bucketed, which is mapped to a feature unique key.

#### Parameters

| Parameter  | Type   | Required | Description                                                                                                                                                 |
| ---------- | ------ | -------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| featureKey | string | Yes      | A feature key                                                                                                                                               |
| attributes | object | No       | An object that specifies attributes for the user. Accepts 5 properties:                                                                                     |
|            |        |          | `locationProperties` an object of key-value pairs that are used for location matching.                                                                      |
|            |        |          | `visitorProperties` an object of key-value pairs that are used for audience targeting.                                                                      |
|            |        |          | `updateVisitorProperties` optional boolean for updating in-memory visitor properties.                                                                       |
|            |        |          | `environment` optional string.                                                                                                                              |
|            |        |          | `typeCasting` optional boolean to dontrol automatic type conversion to the variable's defined type. Does not do any JSON validation (_defaults to `true`_). |
|            |        |          | `experienceKeys` optional array of strings to use only specified experiences.                                                                               |

#### Returns

Bucketed feature.

#### Example

```typescript
import ConvertSDK, {
  ConvertInterface,
  ConvertConfig,
  ContextInterface,
  BucketedFeature
} from '@convertcom/js-sdk';

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

### Track Conversion Event

Decides whether to send a conversion event, which is mapped to a goal-unique key. The decision is made against the configured goal-triggering rules.

#### Parameters

| Parameter  | Type   | Required | Description                                                                         |
| ---------- | ------ | -------- | ----------------------------------------------------------------------------------- |
| goalKey    | string | Yes      | A goal key                                                                          |
| attributes | object | No       | An object that specifies attributes for the user. Accepts 2 properties:             |
|            |        |          | `ruleData` an object of key-value pairs that are used for location matching.        |
|            |        |          | `conversionData` an object of key-value pairs that are used for audience targeting. |

#### Returns

Void.

#### Example

```typescript
import ConvertSDK, {
  ConvertInterface,
  ConvertConfig,
  ContextInterface
} from '@convertcom/js-sdk';

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
        amount: 10.3,
        productsCount: 2
      }
    ]
  });
});
```

### Run Custom Segments

Decides whether to update custom segments in user context, which is mapped to segments' unique keys. The decision is made against the configured segment rules.

#### Parameters

| Parameter         | Type   | Required | Description                                                       |
| ----------------- | ------ | -------- | ----------------------------------------------------------------- |
| segmentsKeys      | array  | Yes      | A list of segments keys                                           |
| visitorProperties | object | No       | An object of key-value pairs that are used for segments matching. |

#### Returns

Void.

#### Example

```typescript
import ConvertSDK, {
  ConvertInterface,
  ConvertConfig,
  ContextInterface
} from '@convertcom/js-sdk';

const config: ConvertConfig = {
  // full configuration options
};

const convertSDK: ConvertInterface = new ConvertSDK(config);
convertSDK.onReady().then(() => {
  const context: ContextInterface = convertSDK.createContext('user-unique-id');
  context.runCustomSegments(['segments-key'], {
    enabled: true
  });
});
```

### Set Default Segments

Permanently update the visitor segments for reporting purposes. Only the following properties are included in Convert Reports: `browser`, `devices`, `source`, `campaign`, `visitorType`, and `country`.

#### Parameters

| Parameter | Type   | Required | Description                                                                                                                 |
| --------- | ------ | -------- | --------------------------------------------------------------------------------------------------------------------------- |
| segments  | object | Yes      | An object of key-value pairs to be merged with the initial **User Properties** created with [context](#create-user-context) |

#### Returns

Void.

#### Example

```typescript
import ConvertSDK, {
  ConvertInterface,
  ConvertConfig,
  ContextInterface
} from '@convertcom/js-sdk';

const config: ConvertConfig = {
  // full configuration options
};

const convertSDK: ConvertInterface = new ConvertSDK(config);
convertSDK.onReady().then(() => {
  const context: ContextInterface = convertSDK.createContext('user-unique-id');
  context.setDefaultSegments({country: 'US'});
});
```

### Update Visitor Properties

Permanently update all of the visitor properties used inside Audience definition evaluation.

#### Parameters

| Parameter         | Type   | Required | Description                                                                                                                 |
| ----------------- | ------ | -------- | --------------------------------------------------------------------------------------------------------------------------- |
| visitorId         | string | Yes      | User unique ID                                                                                                              |
| visitorProperties | object | Yes      | An object of key-value pairs to be merged with the initial **User Properties** created with [context](#create-user-context) |

#### Returns

Void.

#### Example

```typescript
import ConvertSDK, {
  ConvertInterface,
  ConvertConfig,
  ContextInterface
} from '@convertcom/js-sdk';

const config: ConvertConfig = {
  // full configuration options
};

const convertSDK: ConvertInterface = new ConvertSDK(config);
convertSDK.onReady().then(() => {
  const context: ContextInterface = convertSDK.createContext('user-unique-id');
  context.updateVisitorProperties({weather: 'rainy'});
});
```

### Get Config Entity

Find a single entity in configutation by `key`

#### Parameters

| Parameter  | Type       | Required | Description                                                                                                                                                                                                |
| ---------- | ---------- | -------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| key        | string     | Yes      | Entity key as found in configuration                                                                                                                                                                       |
| entityType | EntityType | Yes      | One of the configuration entities: `EntityType.AUDIENCES`, `EntityType.LOCATIONS`, `EntityType.SEGMENTS`, `EntityType.FEATURES`, `EntityType.GOALS`, `EntityType.EXPERIENCES`, and `EntityType.VARIATIONS` |

#### Returns

Void.

#### Example

```typescript
import ConvertSDK, {
  ConvertInterface,
  ConvertConfig,
  ContextInterface,
  BucketedVariation,
  EntityType,
  Feature
} from '@convertcom/js-sdk';

const config: ConvertConfig = {
  // full configuration options
};

const convertSDK: ConvertInterface = new ConvertSDK(config);
convertSDK.onReady().then(() => {
  const context: ContextInterface = convertSDK.createContext('user-unique-id');
  const variation: BucketedVariation =
    userContext.runExperience('experience-key');
  const feature: Feature = userContext.getConfigEntity(
    variation.key,
    EntityType.FEATURES
  );
});
```

### Get Config Entity by Id

Find a single entity in configutation by `id`

#### Parameters

| Parameter  | Type       | Required | Description                                                                                                                                                                                                |
| ---------- | ---------- | -------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------- |
| id         | string     | number   | Yes                                                                                                                                                                                                        | Entity id as found in configuration |
| entityType | EntityType | Yes      | One of the configuration entities: `EntityType.AUDIENCES`, `EntityType.LOCATIONS`, `EntityType.SEGMENTS`, `EntityType.FEATURES`, `EntityType.GOALS`, `EntityType.EXPERIENCES`, and `EntityType.VARIATIONS` |

#### Returns

Void.

#### Example

```typescript
import ConvertSDK, {
  ConvertInterface,
  ConvertConfig,
  ContextInterface,
  BucketedVariation,
  EntityType,
  Feature
} from '@convertcom/js-sdk';

const config: ConvertConfig = {
  // full configuration options
};

const convertSDK: ConvertInterface = new ConvertSDK(config);
convertSDK.onReady().then(() => {
  const context: ContextInterface = convertSDK.createContext('user-unique-id');
  const variation: BucketedVariation =
    userContext.runExperience('experience-key');
  const feature: Feature = userContext.getConfigEntityById(
    variation.id,
    EntityType.FEATURES
  );
});
```

### Release Pending Queues

Send all pending API/DataStore queues to server

#### Parameters

| Parameter | Type   | Required | Description                           |
| --------- | ------ | -------- | ------------------------------------- |
| reason    | string | No       | Custom message for debugging purposes |

#### Returns

Void.

#### Example

```typescript
import ConvertSDK, {
  ConvertInterface,
  ConvertConfig,
  ContextInterface,
  BucketedVariation
} from '@convertcom/js-sdk';

const config: ConvertConfig = {
  // full configuration options
};

const convertSDK: ConvertInterface = new ConvertSDK(config);
convertSDK.onReady().then(() => {
  const context: ContextInterface = convertSDK.createContext('user-unique-id');
  const variations: BucketedVariation[] = context.runExperiences();

  // manually release all pending queue at some point later, like on click, component unmount
  context.releaseQueues();
});
```

### Events

You can capture SDK events as well:

| Event                  | Triggered by                                             | Callback data                                                                            |
| ---------------------- | -------------------------------------------------------- | ---------------------------------------------------------------------------------------- |
| `ready`                | Initializing the SDK                                     | null                                                                                     |
| `bucketing`            | Run experience(s)                                        | { visitorId: `string`, experienceKey: `string`, variationKey: `string` }                 |
|                        | Run feature(s)                                           | { visitorId: `string`, experienceKey: `string`, featureKey: `string`, status: `string` } |
| `conversion`           | Track conversion                                         | { visitorId: `string`, goalKey: `string` }                                               |
| `location.activated`   | Location rules matched                                   | { visitorId: `string`, location: { id: `string`, name: `string`, key: `string` } }       |
| `location.deactivated` | Location rules not matched (_only if activated earlier_) | { visitorId: `string`, location: { id: `string`, name: `string`, key: `string` } }       |
| `config.updated`       | Refreshing the configuration                             | null                                                                                     |

```typescript
import ConvertSDK, {
  ConvertInterface,
  ConvertConfig,
  ContextInterface,
  EntityType,
  Experience,
  SystemEvents,
  Variation
} from '@convertcom/js-sdk';

const convertSDK: ConvertInterface = new ConvertSDK({sdkKey: 'xxx'} as ConvertConfig);

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
      // Exmaple GA integrations can be done here
      // note that you need to create audiences manually at GA side
      const {name: experienceName} as Experience = context.getConfigEntity(
        experienceKey,
        EntityType.EXPERIENCES
      );
      const {name: variationName} as Variation = context.getConfigEntity(
        variationKey,
        EntityType.VARIATIONS
      );
      gtag('event', 'YOUR_GA_CUSOTM_EVENT', {experienceName, variationName});
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

> **Note**: This feature is optional

You can provide your own DataStore that is used to make user bucketing persistent, ensuring persistent experience variation selection, regardless of the experience’s configurations changing or not. As long as Experience variations and/or traffic allocation do not change, the initial user bucketing remains persistent (We use the [MurmurHash](https://en.wikipedia.org/wiki/MurmurHash) algorithm to decide which variation will be selected. This ensures the same one is going to be selected each time, as long as the experience’s configuration does not change.)

The provided DataStore interface is expected to provide 2 methods: `set` and `get`.

```typescript
import ConvertSDK, {
  ConvertInterface,
  ConvertConfig,
  ContextInterface
} from '@convertcom/js-sdk';

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

## Build Environment Variables

| Environment Variable | Description                                                                                                                          | Value                                                                     |
| -------------------- | ------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------- |
| `LOG_LEVEL`          | Number prepresents the level of log statements to keep, while removing the rest from bundle output (_defaults to include all logs_). | `0` = ALL, `1` = DEBUG, `2` = INFO, `3` = WARN, `4` = ERROR, `5` = SILENT |
| `BUNDLES`            | Comma-separated tokens for specifying which bundles to build (_defaults to include all bundles_).                                    | `cjs`, `cjs-legacy`, `esm`, `umd`                                         |

## Custom Build

You can use a customized build in certain situations. For example:

1. Reducing bundle size and remove all log statements: `LOG_LEVEL=5 yarn sdk:build`
2. Build CommonJS bundles only: `BUNDLES=cjs,cjs-legacy yarn sdk:build`

Additionaly, you can even include this repository as part of your own `TypeScript` project:

1. Add Convert JavaScriptSDK repository to your own as a submodule: `git submodule add https://github.com/convertcom/javascript-sdk javascript-sdk`
2. Add the following `workspaces` to your `package.json` (_assuming that the submodule located under you repository root_):
   ```json
   "workspaces": [
     "javascript-sdk/packages/*"
   ]
   ```
3. If using PnP, the following `packages` to your `pnpm-workspace.yaml` instead:
   ```yaml
   packages:
     - 'javascript-sdk/packages/*'
   ```
4. Add the following `paths` under `compilerOptions` of your `tsconfig.json`:
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
         "@convertcom/js-sdk-segments": ["./javascript-sdk/packages/segments"],
         "@convertcom/js-sdk-types": ["./javascript-sdk/packages/types"],
         "@convertcom/js-sdk-utils": ["./javascript-sdk/packages/utils"],
         "@convertcom/js-sdk": ["./javascript-sdk/packages/js-sdk"]
       }
     }
   }
   ```
5. Add the following script to your `package.json`:
   ```json
   "scripts": {
     "build:sdk": "cd javascript-sdk && BUNDLES=cjs,esm LOG_LEVEL=5 yarn sdk:build"
   }
   ```
   > Note that both `BUNDLES` and `LOG_LEVEL` are optional (_see [Build Environment Variables](#build-environment-variables) above_)
6. Now you can build Convert JavaScriptSDK alongside your project (_assuming that you use `rollup` for bundling_):
   ```json
   "scripts": {
     "build": "yarn build:sdk && rollup -c"
   }
   ```
7. You need to run `yarn` inside the submodule `javascript-sdk` as well:
   ```bash
   # update ConvertSDK submodule
   git submodule update --init --remote
   # install your project dependencies at your `package.json`
   yarn
   # install ConvertSDK dependencies
   cd javascript-sdk && yarn
   # build your project
   yarn build
   ```

## Credits

Copyright(c) 2024 Convert Insights, Inc.
