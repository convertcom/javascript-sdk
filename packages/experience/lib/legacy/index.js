'use strict';

var jsSdkEnums = require('@convertcom/js-sdk-enums');

/**
 * Provides experiences specific logic
 * @category Modules
 * @constructor
 * @implements {ExperienceManagerInterface}
 */
var ExperienceManager = /** @class */ (function () {
    /**
     * @param config
     * @param {Record<string, any>} dependencies
     * @param {DataManagerInterface} dependencies.dataManager
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    function ExperienceManager(config, _a) {
        var dataManager = _a.dataManager, loggerManager = _a.loggerManager;
        var _b, _c;
        this._dataManager = dataManager;
        this._loggerManager = loggerManager;
        (_c = (_b = this._loggerManager) === null || _b === void 0 ? void 0 : _b.trace) === null || _c === void 0 ? void 0 : _c.call(_b, jsSdkEnums.MESSAGES.EXPERIENCE_CONSTRUCTOR);
    }
    /**
     * Get a list of all entities
     * @return {Array<Experience>} Experiences list
     */
    ExperienceManager.prototype.getList = function () {
        return this._dataManager.getEntitiesList('experiences');
    };
    /**
     * Get the entity by key
     * @param {string} key
     * @return {Experience} An experience
     */
    ExperienceManager.prototype.getExperience = function (key) {
        return this._dataManager.getEntity(key, 'experiences');
    };
    /**
     * Get the entity by id
     * @param {Id} id
     * @return {Experience} Get single experience
     */
    ExperienceManager.prototype.getExperienceById = function (id) {
        return this._dataManager.getEntityById(id, 'experiences');
    };
    /**
     * Get specific entities by array of keys
     * @param {Array<string>} keys
     * @return {Array<Experience>}
     */
    ExperienceManager.prototype.getExperiences = function (keys) {
        return this._dataManager.getItemsByKeys(keys, 'experiences');
    };
    /**
     * Select variation for specific visitor
     * @param {Id} visitorId
     * @param {string} experienceKey
     * @param {Record<string, any> | null} visitorProperties
     * @param {Record<string, any> | null} locationProperties
     * @param {string=} environment
     * @return {BucketedVariation | RuleError}
     */
    ExperienceManager.prototype.selectVariation = function (visitorId, experienceKey, visitorProperties, locationProperties, environment) {
        return this._dataManager.getBucketing(visitorId, experienceKey, visitorProperties, locationProperties, environment);
    };
    /**
     * Select variation for specific visitor
     * @param {Id} visitorId
     * @param {Id} experienceId
     * @param {Record<string, any> | null} visitorProperties
     * @param {Record<string, any> | null} locationProperties
     * @param {string=} environment
     * @return {BucketedVariation | RuleError}
     */
    ExperienceManager.prototype.selectVariationById = function (visitorId, experienceId, visitorProperties, // TODO: proceed if null as if visitorProperties matched
    locationProperties, // TODO: proceed if null as if locationProperties matched
    environment) {
        return this._dataManager.getBucketingById(visitorId, experienceId, visitorProperties, locationProperties, environment);
    };
    /**
     * Select all variations across all experiences for specific visitor
     * @param {Id} visitorId
     * @param {Record<string, any> | null} visitorProperties
     * @param {Record<string, any> | null} locationProperties
     * @param {string=} environment
     * @return {Array<BucketedVariation | RuleError>}
     */
    ExperienceManager.prototype.selectVariations = function (visitorId, visitorProperties, locationProperties, environment) {
        var _this = this;
        return this.getList()
            .map(function (experience) {
            return _this.selectVariation(visitorId, experience === null || experience === void 0 ? void 0 : experience.key, visitorProperties, locationProperties, environment);
        })
            .filter(Boolean);
    };
    /**
     * Get experience's variation by key
     * @param {string} experienceKey
     * @param {string}variationKey
     */
    ExperienceManager.prototype.getVariation = function (experienceKey, variationKey) {
        return this._dataManager.getSubItem('experiences', experienceKey, 'variations', variationKey, 'key', 'key');
    };
    /**
     * Get experience's variation by id
     * @param experienceId
     * @param variationId
     */
    ExperienceManager.prototype.getVariationById = function (experienceId, variationId) {
        return this._dataManager.getSubItem('experiences', experienceId, 'variations', variationId, 'id', 'id');
    };
    return ExperienceManager;
}());

exports.ExperienceManager = ExperienceManager;
//# sourceMappingURL=index.js.map
