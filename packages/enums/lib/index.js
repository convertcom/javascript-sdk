'use strict';

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
exports.AudienceType = void 0;
(function (AudienceType) {
    AudienceType["PERMANENT"] = "permanent";
    AudienceType["SEGMENTATION"] = "segmentation";
    AudienceType["TRANSIENT"] = "transient";
})(exports.AudienceType || (exports.AudienceType = {}));

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
const DATA_ENTITIES = [
    'events',
    'goals',
    'audiences',
    'locations',
    'segments',
    'experiences',
    'archived_experiences',
    'experiences.variations',
    'features',
    'features.variables'
];

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
const ERROR_MESSAGES = {
    SDK_KEY_MISSING: 'SDK key is missing',
    DATA_OBJECT_MISSING: 'Data object is missing',
    CONFIG_DATA_NOT_VALID: 'Config Data is not valid',
    SDK_OR_DATA_OBJECT_REQUIRED: 'SDK key or Data object should be provided',
    RULE_NOT_VALID: 'Provided rule is not valid',
    RULE_DATA_NOT_VALID: 'Provided rule data is not valid',
    DATA_STORE_NOT_VALID: 'DataStore object is not valid. It should contain get and set methods',
    VISITOR_ID_REQUIRED: 'Visitor Id string is not present',
    GOAL_DATA_NOT_VALID: 'GoalData object is not valid',
    UNABLE_TO_SELECT_BUCKET_FOR_VISITOR: 'Unable to bucket visitor',
    UNABLE_TO_PERFORM_NETWORK_REQUEST: 'Unable to perform network request',
    UNSUPPORTED_RESPONSE_TYPE: 'Unsupported response type'
};
const MESSAGES = {
    CONFIG_DATA_UPDATED: 'Config Data updated',
    CORE_CONSTRUCTOR: 'Core Manager constructor has been called',
    CORE_INITIALIZED: 'Core Manager has been initialized',
    EXPERIENCE_CONSTRUCTOR: 'Experience Manager constructor has been called',
    VARIATIONS_NOT_FOUND: 'Variations not found',
    VARIATION_CHANGE_NOT_SUPPORTED: 'Variation change not supported',
    FEATURE_CONSTRUCTOR: 'Feature Manager constructor has been called',
    FEATURE_NOT_FOUND: 'Fullstack Feature not found',
    FEATURE_VARIABLES_NOT_FOUND: 'Fullstack Feature Variables not found',
    FEATURE_VARIABLES_TYPE_NOT_FOUND: 'Fullstack Feature Variables Type not found',
    BUCKETING_CONSTRUCTOR: 'Bucketing Manager constructor has been called',
    DATA_CONSTRUCTOR: 'Data Manager constructor has been called',
    RULE_CONSTRUCTOR: 'Rule Manager constructor has been called',
    LOCATION_NOT_MATCH: 'Location does not match',
    RULES_NOT_MATCH: 'Rules do not match',
    RULES_MATCH: 'Found matched rules',
    BUCKETED_VISITOR_FOUND: 'Visitor is already bucketed and the variation is found',
    GOAL_NOT_FOUND: 'Goal not found',
    GOAL_RULE_NOT_MATCH: 'Goal rule do not match',
    SEGMENTS_NOT_FOUND: 'Segments not found',
    SEGMENTS_RULE_NOT_MATCH: 'Segments rule do not match',
    CUSTOM_SEGMENTS_KEY_FOUND: 'Custom segments key already set',
    SEND_BEACON_SUCCESS: 'The user agent successfully queued the data for transfer'
};

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
exports.EventType = void 0;
(function (EventType) {
    EventType["BUCKETING"] = "bucketing";
    EventType["CONVERSION"] = "conversion";
})(exports.EventType || (exports.EventType = {}));

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
exports.ExperienceStatus = void 0;
(function (ExperienceStatus) {
    ExperienceStatus["DRAFT"] = "draft";
    ExperienceStatus["ACTIVE"] = "active";
    ExperienceStatus["PAUSED"] = "paused";
    ExperienceStatus["COMPLETED"] = "completed";
    ExperienceStatus["SCHEDULED"] = "scheduled";
    ExperienceStatus["ARCHIVED"] = "archived";
    ExperienceStatus["DELETED"] = "deleted";
})(exports.ExperienceStatus || (exports.ExperienceStatus = {}));

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
exports.ExperienceType = void 0;
(function (ExperienceType) {
    ExperienceType["AB"] = "a/b";
    ExperienceType["AA"] = "a/a";
    ExperienceType["MVT"] = "mvt";
    ExperienceType["SPLIT"] = "split_url";
    ExperienceType["DEPLOY"] = "deploy";
    ExperienceType["MULTIPAGE"] = "multipage";
    ExperienceType["AB_FULLSTACK"] = "a/b_fullstack";
    ExperienceType["FEATURE_ROLLOUT"] = "feature_rollout";
})(exports.ExperienceType || (exports.ExperienceType = {}));

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
exports.FeatureStatus = void 0;
(function (FeatureStatus) {
    FeatureStatus["ENABLED"] = "enabled";
    FeatureStatus["DISABLED"] = "disabled";
})(exports.FeatureStatus || (exports.FeatureStatus = {}));

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
exports.GoalDataKey = void 0;
(function (GoalDataKey) {
    GoalDataKey["AMOUNT"] = "amount";
    GoalDataKey["PRODUCTS_COUNT"] = "productsCount";
})(exports.GoalDataKey || (exports.GoalDataKey = {}));

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
exports.GoalType = void 0;
(function (GoalType) {
    GoalType["ADVANCED"] = "advanced";
    GoalType["DOM_INTERACTION"] = "dom_interaction";
    GoalType["SCROLL_PERCENTAGE"] = "scroll_percentage";
    GoalType["CODE_TRIGGER"] = "code_trigger";
    GoalType["REVENUE"] = "revenue";
})(exports.GoalType || (exports.GoalType = {}));

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
exports.LogLevel = void 0;
(function (LogLevel) {
    LogLevel[LogLevel["TRACE"] = 0] = "TRACE";
    LogLevel[LogLevel["DEBUG"] = 1] = "DEBUG";
    LogLevel[LogLevel["INFO"] = 2] = "INFO";
    LogLevel[LogLevel["WARN"] = 3] = "WARN";
    LogLevel[LogLevel["ERROR"] = 4] = "ERROR";
    LogLevel[LogLevel["SILENT"] = 5] = "SILENT";
})(exports.LogLevel || (exports.LogLevel = {}));

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
exports.LogMethod = void 0;
(function (LogMethod) {
    LogMethod["LOG"] = "log";
    LogMethod["TRACE"] = "trace";
    LogMethod["DEBUG"] = "debug";
    LogMethod["INFO"] = "info";
    LogMethod["WARN"] = "warn";
    LogMethod["ERROR"] = "error";
})(exports.LogMethod || (exports.LogMethod = {}));

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
exports.ProjectType = void 0;
(function (ProjectType) {
    ProjectType["WEB"] = "web";
    ProjectType["FULLSTACK"] = "fullstack";
})(exports.ProjectType || (exports.ProjectType = {}));

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
exports.RuleError = void 0;
(function (RuleError) {
    RuleError["NO_DATA_FOUND"] = "convert.com_no_data_found";
    RuleError["NEED_MORE_DATA"] = "convert.com_need_more_data";
})(exports.RuleError || (exports.RuleError = {}));

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
/**
 * SDK system events. Possible values: 'ready' | 'queue-released'
 * or custom visitor's event
 */
exports.SystemEvents = void 0;
(function (SystemEvents) {
    SystemEvents["READY"] = "ready";
    SystemEvents["CONFIG_UPDATED"] = "config-updated";
    SystemEvents["API_QUEUE_RELEASED"] = "api-queue-released";
    SystemEvents["BUCKETING"] = "bucketing";
    SystemEvents["CONVERSION"] = "conversion";
    SystemEvents["SEGMENTS"] = "segments";
    SystemEvents["DATA_STORE_QUEUE_RELEASED"] = "data-store-queue-released";
})(exports.SystemEvents || (exports.SystemEvents = {}));

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
const VARIABLE_TYPES = [
    'boolean',
    'float',
    'json',
    'integer',
    'string'
];

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
exports.VariationStatus = void 0;
(function (VariationStatus) {
    VariationStatus["STOPPED"] = "stopped";
    VariationStatus["ACTIVE"] = "active";
    VariationStatus["RUNNING"] = "running";
    VariationStatus["ARCHIVED"] = "archived";
})(exports.VariationStatus || (exports.VariationStatus = {}));

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
exports.VariationType = void 0;
(function (VariationType) {
    VariationType["RICH_STRUCTURE"] = "richStructure";
    VariationType["CUSTOM_CODE"] = "customCode";
    VariationType["DEFAULT_CODE"] = "defaultCode";
    VariationType["DEFAULT_CODE_MULTIPLE"] = "defaultCodeMultipage";
    VariationType["DEFAULT_REDIRECT"] = "defaultRedirect";
    VariationType["FULLSTACK_FEATURE"] = "fullStackFeature";
})(exports.VariationType || (exports.VariationType = {}));

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
exports.BrowserType = void 0;
(function (BrowserType) {
    BrowserType["IE"] = "IE";
    BrowserType["CH"] = "CH";
    BrowserType["FF"] = "FF";
    BrowserType["OP"] = "OP";
    BrowserType["SF"] = "SF";
    BrowserType["OTH"] = "OTH";
})(exports.BrowserType || (exports.BrowserType = {}));

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
exports.DeviceType = void 0;
(function (DeviceType) {
    DeviceType["ALLPH"] = "ALLPH";
    DeviceType["IPH"] = "IPH";
    DeviceType["OTHPH"] = "OTHPH";
    DeviceType["ALLTAB"] = "ALLTAB";
    DeviceType["IPAD"] = "IPAD";
    DeviceType["OTHTAB"] = "OTHTAB";
    DeviceType["DESK"] = "DESK";
    DeviceType["OTHDEV"] = "OTHDEV";
})(exports.DeviceType || (exports.DeviceType = {}));

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
exports.SegmentsKeys = void 0;
(function (SegmentsKeys) {
    SegmentsKeys["COUNTRY"] = "country";
    SegmentsKeys["BROWSER"] = "browser";
    SegmentsKeys["DEVICES"] = "devices";
    SegmentsKeys["SOURCE"] = "source";
    SegmentsKeys["CAMPAIGN"] = "campaign";
    SegmentsKeys["VISITOR_TYPE"] = "visitorType";
    SegmentsKeys["CUSTOM_SEGMENTS"] = "customSegments";
})(exports.SegmentsKeys || (exports.SegmentsKeys = {}));

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
exports.SourceType = void 0;
(function (SourceType) {
    SourceType["CAMPAIGN"] = "campaign";
    SourceType["SEARCH"] = "search";
    SourceType["REFERRAL"] = "referral";
    SourceType["DIRECT"] = "direct";
})(exports.SourceType || (exports.SourceType = {}));

/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
exports.VisitorType = void 0;
(function (VisitorType) {
    VisitorType["NEW"] = "new";
    VisitorType["RETURNING"] = "returning";
})(exports.VisitorType || (exports.VisitorType = {}));

exports.DATA_ENTITIES = DATA_ENTITIES;
exports.ERROR_MESSAGES = ERROR_MESSAGES;
exports.MESSAGES = MESSAGES;
exports.VARIABLE_TYPES = VARIABLE_TYPES;
//# sourceMappingURL=index.js.map
