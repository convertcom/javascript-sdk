/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { BucketingEvent } from './BucketingEvent';
import type { ConversionEvent } from './ConversionEvent';
/**
 * Tracking events related to the same user ID
 */
export type VisitorTrackingEvents = {
    /**
     * Type of the event. It can be a bucketing or a conversion event
     */
    eventType?: VisitorTrackingEvents.eventType;
    data?: (BucketingEvent | ConversionEvent);
};
export namespace VisitorTrackingEvents {
    /**
     * Type of the event. It can be a bucketing or a conversion event
     */
    export enum eventType {
        BUCKETING = 'bucketing',
        CONVERSION = 'conversion',
    }
}

