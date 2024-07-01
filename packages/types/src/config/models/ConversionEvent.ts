/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
/**
 * Conversion event data
 */
export type ConversionEvent = {
    /**
     * Id of the conversion goal to be fired
     */
    goalId: string;
    /**
     * Data connected to this conversion, for non binomial metrics, eg revenue
     */
    goalData?: Array<{
        /**
         * Key of the metric
         */
        key?: 'amount' | 'productsCount' | 'transactionId';
        /**
         * Value of the metric
         */
        value?: (number | string);
    }>;
    /**
     * Bucketing data (experiences that this visitor is currently part of) for the visitor. In case that **enrichData=true** flag is being sent and
     * this attribute is not provided, the bucketing stored on the backend datastore for the given visitor is gonna be used. If both **enrichData=true** and
     * **bucketingData**, the **bucketingData** is gonna be merged with the stored data inside the backend data source, the request provided data having the
     * biggest overwriting bucketing for the same experience which might exist on the backend
     *
     */
    bucketingData?: Record<string, string>;
};

