/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import { DataManagerInterface } from '@convertcom/data';
import { ExperienceManagerInterface } from './interfaces/experience-manager';
import { LogManagerInterface } from '@convertcom/logger';
import { Config, Experience, Id, Variation, BucketedVariation } from '@convertcom/types';
import { RuleError } from '@convertcom/enums';
/**
 * Provides experiences specific logic
 * @category Modules
 * @constructor
 * @implements {ExperienceManagerInterface}
 */
export declare class ExperienceManager implements ExperienceManagerInterface {
    private _dataManager;
    private _loggerManager;
    /**
     * @param config
     * @param {Record<string, any>} dependencies
     * @param {DataManagerInterface} dependencies.dataManager
     * @param {LogManagerInterface=} dependencies.loggerManager
     */
    constructor(config: Config, { dataManager, loggerManager }: {
        dataManager: DataManagerInterface;
        loggerManager?: LogManagerInterface;
    });
    /**
     * Get a list of all entities
     * @return {Array<Experience>} Experiences list
     */
    getList(): Array<Experience>;
    /**
     * Get the entity by key
     * @param {string} key
     * @return {Experience} An experience
     */
    getExperience(key: string): Experience;
    /**
     * Get the entity by id
     * @param {Id} id
     * @return {Experience} Get single experience
     */
    getExperienceById(id: Id): Experience;
    /**
     * Get specific entities by array of keys
     * @param {Array<string>} keys
     * @return {Array<Experience>}
     */
    getExperiences(keys: Array<string>): Array<Experience>;
    /**
     * Select variation for specific visitor
     * @param {Id} visitorId
     * @param {string} experienceKey
     * @param {Record<string, any> | null} visitorProperties
     * @param {Record<string, any> | null} locationProperties
     * @param {string=} environment
     * @return {BucketedVariation | RuleError}
     */
    selectVariation(visitorId: Id, experienceKey: string, visitorProperties: Record<string, any> | null, locationProperties: Record<string, any> | null, environment?: string): BucketedVariation | RuleError;
    /**
     * Select variation for specific visitor
     * @param {Id} visitorId
     * @param {Id} experienceId
     * @param {Record<string, any> | null} visitorProperties
     * @param {Record<string, any> | null} locationProperties
     * @param {string=} environment
     * @return {BucketedVariation | RuleError}
     */
    selectVariationById(visitorId: Id, experienceId: Id, visitorProperties: Record<string, any> | null, // TODO: proceed if null as if visitorProperties matched
    locationProperties: Record<string, any> | null, // TODO: proceed if null as if locationProperties matched
    environment?: string): BucketedVariation | RuleError;
    /**
     * Select all variations across all experiences for specific visitor
     * @param {Id} visitorId
     * @param {Record<string, any> | null} visitorProperties
     * @param {Record<string, any> | null} locationProperties
     * @param {string=} environment
     * @return {Array<BucketedVariation | RuleError>}
     */
    selectVariations(visitorId: Id, visitorProperties: Record<string, any> | null, locationProperties: Record<string, any> | null, environment?: string): Array<BucketedVariation | RuleError>;
    /**
     * Get experience's variation by key
     * @param {string} experienceKey
     * @param {string}variationKey
     */
    getVariation(experienceKey: string, variationKey: string): Variation;
    /**
     * Get experience's variation by id
     * @param experienceId
     * @param variationId
     */
    getVariationById(experienceId: Id, variationId: Id): Variation;
}
