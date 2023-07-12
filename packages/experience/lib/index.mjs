import { MESSAGES } from '@convertcom/js-sdk-enums';

/**
 * Provides experiences specific logic
 * @category Modules
 * @constructor
 * @implements {ExperienceManagerInterface}
 */
class ExperienceManager {
    /**
     * @param config
     * @param {Record<string, any>} dependencies
     * @param {DataManagerInterface} dependencies.dataManager
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    constructor(config, { dataManager, loggerManager }) {
        var _a, _b;
        this._dataManager = dataManager;
        this._loggerManager = loggerManager;
        (_b = (_a = this._loggerManager) === null || _a === void 0 ? void 0 : _a.trace) === null || _b === void 0 ? void 0 : _b.call(_a, MESSAGES.EXPERIENCE_CONSTRUCTOR);
    }
    /**
     * Get a list of all entities
     * @return {Array<Experience>} Experiences list
     */
    getList() {
        return this._dataManager.getEntitiesList('experiences');
    }
    /**
     * Get the entity by key
     * @param {string} key
     * @return {Experience} An experience
     */
    getExperience(key) {
        return this._dataManager.getEntity(key, 'experiences');
    }
    /**
     * Get the entity by id
     * @param {Id} id
     * @return {Experience} Get single experience
     */
    getExperienceById(id) {
        return this._dataManager.getEntityById(id, 'experiences');
    }
    /**
     * Get specific entities by array of keys
     * @param {Array<string>} keys
     * @return {Array<Experience>}
     */
    getExperiences(keys) {
        return this._dataManager.getItemsByKeys(keys, 'experiences');
    }
    /**
     * Select variation for specific visitor
     * @param {Id} visitorId
     * @param {string} experienceKey
     * @param {Record<string, any> | null} visitorProperties
     * @param {Record<string, any> | null} locationProperties
     * @param {string=} environment
     * @return {BucketedVariation | RuleError}
     */
    selectVariation(visitorId, experienceKey, visitorProperties, locationProperties, environment) {
        return this._dataManager.getBucketing(visitorId, experienceKey, visitorProperties, locationProperties, environment);
    }
    /**
     * Select variation for specific visitor
     * @param {Id} visitorId
     * @param {Id} experienceId
     * @param {Record<string, any> | null} visitorProperties
     * @param {Record<string, any> | null} locationProperties
     * @param {string=} environment
     * @return {BucketedVariation | RuleError}
     */
    selectVariationById(visitorId, experienceId, visitorProperties, // TODO: proceed if null as if visitorProperties matched
    locationProperties, // TODO: proceed if null as if locationProperties matched
    environment) {
        return this._dataManager.getBucketingById(visitorId, experienceId, visitorProperties, locationProperties, environment);
    }
    /**
     * Select all variations across all experiences for specific visitor
     * @param {Id} visitorId
     * @param {Record<string, any> | null} visitorProperties
     * @param {Record<string, any> | null} locationProperties
     * @param {string=} environment
     * @return {Array<BucketedVariation | RuleError>}
     */
    selectVariations(visitorId, visitorProperties, locationProperties, environment) {
        return this.getList()
            .map((experience) => {
            return this.selectVariation(visitorId, experience === null || experience === void 0 ? void 0 : experience.key, visitorProperties, locationProperties, environment);
        })
            .filter(Boolean);
    }
    /**
     * Get experience's variation by key
     * @param {string} experienceKey
     * @param {string}variationKey
     */
    getVariation(experienceKey, variationKey) {
        return this._dataManager.getSubItem('experiences', experienceKey, 'variations', variationKey, 'key', 'key');
    }
    /**
     * Get experience's variation by id
     * @param experienceId
     * @param variationId
     */
    getVariationById(experienceId, variationId) {
        return this._dataManager.getSubItem('experiences', experienceId, 'variations', variationId, 'id', 'id');
    }
}

export { ExperienceManager };
//# sourceMappingURL=index.mjs.map
