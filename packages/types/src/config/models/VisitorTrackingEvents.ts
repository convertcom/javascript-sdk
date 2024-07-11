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
    eventType?: 'bucketing' | 'conversion';
    data?: (BucketingEvent | ConversionEvent);
}