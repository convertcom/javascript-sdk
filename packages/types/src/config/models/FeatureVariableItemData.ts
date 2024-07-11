/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

/**
 * An object which describes the variable of a feature. Where key is variable name and value is one of the possible types [boolean, float, json, integer, string]
 */
export type FeatureVariableItemData = {
    /**
     * A user-defined variable name
     */
    key?: string;
    /**
     * A variable's type
     */
    type?: 'boolean' | 'float' | 'json' | 'integer' | 'string';
}