import { ApiManagerInterface } from '@convertcom/js-sdk-api';
import { BucketingManagerInterface } from '@convertcom/js-sdk-bucketing';
import { DataStoreManagerInterface } from './interfaces/data-store-manager';
import { DataManagerInterface } from './interfaces/data-manager';
import { EventManagerInterface } from '@convertcom/js-sdk-event';
import { LogManagerInterface } from '@convertcom/js-sdk-logger';
import { RuleManagerInterface } from '@convertcom/js-sdk-rules';
import { Entity, Id, Config, ConfigData, IdentityField, BucketedVariation, StoreData, SegmentsData } from '@convertcom/js-sdk-types';
import { RuleError, GoalDataKey } from '@convertcom/js-sdk-enums';
/**
 * Provides logic for data. Stores bucket with help of dataStore if it's provided
 * @category Modules
 * @constructor
 * @implements {DataManagerInterface}
 */
export declare class DataManager implements DataManagerInterface {
    private _data;
    private _accountId;
    private _projectId;
    private _config;
    private _bucketingManager;
    private _loggerManager;
    private _eventManager;
    private _dataStoreManager;
    private _apiManager;
    private _ruleManager;
    private _dataEntities;
    private _localStoreLimit;
    private _bucketedVisitors;
    private _environment;
    /**
     * @param {Config} config
     * @param {Object} dependencies
     * @param {ApiManagerInterface} dependencies.apiManager
     * @param {BucketingManagerInterface} dependencies.bucketingManager
     * @param {RuleManagerInterface} dependencies.ruleManager
     * @param {LogManagerInterface} dependencies.loggerManager
     */
    constructor(config: Config, { bucketingManager, ruleManager, eventManager, apiManager, loggerManager }: {
        bucketingManager: BucketingManagerInterface;
        ruleManager: RuleManagerInterface;
        eventManager: EventManagerInterface;
        apiManager: ApiManagerInterface;
        loggerManager?: LogManagerInterface;
    });
    set data(data: ConfigData);
    /**
     * data getter
     */
    get data(): ConfigData;
    /**
     * dataStoreManager setter
     * @param {any=} dataStore
     */
    set dataStoreManager(dataStore: any);
    /**
     * dataStoreManager getter
     */
    get dataStoreManager(): DataStoreManagerInterface;
    /**
     * Retrieve variation for visitor
     * @param {string} visitorId
     * @param {string|Id} identity Value of the field which name is provided in identityField
     * @param {Record<string, any> | null} visitorProperties
     * @param {Record<string, any> | null} locationProperties
     * @param {IdentityField=} identityField Defaults to 'key'
     * @param {string=} environment
     * @return {BucketedVariation | RuleError}
     * @private
     */
    private _getBucketingByField;
    /**
     * Retrieve bucketing for Visitor
     * @param {Id} visitorId
     * @param {Experience} experience
     * @return {BucketedVariation}
     * @private
     */
    private _retrieveBucketing;
    /**
     * @param {Id} experienceId
     * @param {Id} variationId
     * @return {Variation}
     * @private
     */
    private retrieveVariation;
    /**
     * @param {Id} visitorId
     * @param {StoreData} storeData
     * @private
     */
    putLocalStore(visitorId: Id, storeData: StoreData): void;
    /**
     * @param {Id} visitorId
     * @return {StoreData} variation id
     * @private
     */
    getLocalStore(visitorId: Id): StoreData;
    /**
     * @param {Id} visitorId
     * @return {string} storeKey
     * @private
     */
    getStoreKey(visitorId: Id): string;
    /**
     * Retrieve variation for visitor
     * @param {string} visitorId
     * @param {string} key
     * @param {Record<string, any> | null} visitorProperties
     * @param {Record<string, any> | null} locationProperties
     * @param {string=} environment
     * @return {BucketedVariation | RuleError}
     */
    getBucketing(visitorId: string, key: string, visitorProperties: Record<string, any> | null, locationProperties: Record<string, any> | null, environment?: string): BucketedVariation | RuleError;
    /**
     * Retrieve variation for Visitor
     * @param {string} visitorId
     * @param {Id} id
     * @param {Record<string, any> | null} visitorProperties
     * @param {Record<string, any> | null} locationProperties
     * @param {string=} environment
     * @return {BucketedVariation | RuleError}
     */
    getBucketingById(visitorId: string, id: Id, visitorProperties: Record<string, any> | null, locationProperties: Record<string, any> | null, environment?: string): BucketedVariation | RuleError;
    /**
     * Process conversion event
     * @param {Id} visitorId
     * @param {Id} goalId
     * @param {Record<string, any>=} goalRule An object of key-value pairs that are used for goal matching
     * @param {Array<Record<GoalDataKey, number>>} goalData An array of object of key-value pairs
     * @param {SegmentsData} segments
     */
    convert(visitorId: Id, goalId: Id, goalRule?: Record<string, any>, goalData?: Array<Record<GoalDataKey, number>>, segments?: SegmentsData): RuleError;
    /**
     * Get audiences that meet the visitorProperties
     * @param {Array<Record<any, any>>} items
     * @param {Record<string, any>} visitorProperties
     * @return {Array<Record<string, any> | RuleError>}
     */
    filterMatchedRecordsWithRule(items: Array<Record<string, any>>, visitorProperties: Record<string, any>): Array<Record<string, any> | RuleError>;
    /**
     * Get audiences that meet the custom segments
     * @param {Array<Record<any, any>>} items
     * @param {Id} visitorId
     * @return {Array<Record<string, any>>}
     */
    filterMatchedCustomSegments(items: Array<Record<string, any>>, visitorId: Id): Array<Record<string, any>>;
    /**
     * Get list of data entities
     * @param {string} entityType
     * @return {Array<Entity | Id>}
     */
    getEntitiesList(entityType: string): Array<Entity | Id>;
    /**
     * Get list of data entities grouped by field
     * @param {string} entityType
     * @param {IdentityField=} field
     * @return {Record<string, Entity>}
     */
    getEntitiesListObject(entityType: string, field?: IdentityField): Record<string, Entity>;
    /**
     *
     * @param {string|Id} identity Value of the field which name is provided in identityField
     * @param {string} entityType
     * @param {IdentityField=} identityField Defaults to 'key'
     * @return {Entity}
     * @private
     */
    private _getEntityByField;
    /**
     * Find the entity in list by id
     * @param {string} key
     * @param {string} entityType
     * @return {Entity}
     */
    getEntity(key: string, entityType: string): Entity;
    /**
     * Find the entity in list by keys
     * @param {Array<string>} keys
     * @param {string} entityType
     * @return {Array<Entity>}
     */
    getEntities(keys: Array<string>, entityType: string): Array<Entity>;
    /**
     * Find the entity in list by id
     * @param {Id} id
     * @param {string} entityType
     * @return {Entity}
     */
    getEntityById(id: Id, entityType: string): Entity;
    /**
     * Find the entity in list by ids
     * @param {Array<Id>} ids
     * @param {string} entityType
     * @return {Array<Entity>}
     */
    getEntitiesByIds(ids: Array<Id>, entityType: string): Array<Entity>;
    /**
     * Find the items in list by  keys
     * @param {Array<string>} keys
     * @param {string} path
     * @return {Array<Record<string, any>>}
     */
    getItemsByKeys(keys: Array<string>, path: string): Array<Record<string, any>>;
    /**
     * Find the items in list by ids
     * @param {Array<Id>} ids
     * @param {String} path
     * @return {Array<Record<string, any>>}
     */
    getItemsByIds(ids: Array<Id>, path: string): Array<Record<string, any>>;
    /**
     * Find nested item
     * @param {string} entityType
     * @param {string|number} entityIdentity
     * @param {string} subEntityType
     * @param {string|number} subEntityIdentity
     * @param {IdentityField} identityField
     * @param {IdentityField} subIdentityField
     * @return {Record<any, any>}
     */
    getSubItem(entityType: string, entityIdentity: string | number, subEntityType: string, subEntityIdentity: string | number, identityField: IdentityField, subIdentityField: IdentityField): Record<any, any>;
    /**
     * Validates data object
     * @param data
     * @return {boolean}
     */
    isValidConfigData(data: ConfigData): boolean;
}
