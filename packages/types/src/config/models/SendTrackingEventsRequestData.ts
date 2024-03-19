/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { VisitorSegments } from './VisitorSegments';
import type { VisitorTrackingEvents } from './VisitorTrackingEvents';
/**
 * Tracking Request's data
 */
export type SendTrackingEventsRequestData = {
    /**
     * ID of the account under which the project is setup
     */
    accountId?: string;
    /**
     * ID of the project under which the tracking occurs
     */
    projectId?: string;
    /**
     * Flag to determine whether the data is gonna be enriched before the events are stored for reporting.
     * For example, in case of a conversion event, if this flag is on and bucketing is not provided, the bucketing stored on the backend datastore for the given visitor
     * ID would be used. Same applies for segments.
     *
     * *Note*: this flag is only available for some plans
     *
     */
    enrichData?: boolean;
    /**
     * List of visitors tracked. Each visitor can have multiple events.
     *
     */
    visitors?: Array<{
        segments?: VisitorSegments;
        /**
         * Id of the visitor tracked
         */
        visitorId?: string;
        /**
         * List of events fired for the given visitor
         */
        events?: Array<VisitorTrackingEvents>;
    }>;
};

