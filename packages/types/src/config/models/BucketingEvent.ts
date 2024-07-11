/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

/**
 * Bucketing event data
 */
export type BucketingEvent = {
    /**
     * Experience ID to which the visitor is bucketed. In case that **enrichData=true** flag is being sent, only unique events are gonna be recorded. Otherwise, it's
     * up to the client to ensure that duplicates of the same event for the same visitor do not get sent to the tracking endpoint.
     *
     */
    experienceId: string;
    /**
     * Variation ID corresponding to the experience identified by experienceID, that is assigned to the visitor.
     */
    variationId: string;
}