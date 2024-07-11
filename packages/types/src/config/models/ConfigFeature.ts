/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { FeatureVariableItemData } from './FeatureVariableItemData';

/**
 * Base Feature Object
 */
export type ConfigFeature = {
    /**
     * Feature ID
     */
    id?: string;
    /**
     * A name given to the feature to identify it easily
     */
    name?: string;
    /**
     * A unique per project level identifier
     */
    key?: string;
    /**
     * An array of user-defined variables of a feature.
     */
    variables?: Array<FeatureVariableItemData>;
}