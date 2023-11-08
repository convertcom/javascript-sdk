/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {EventType} from '@convertcom/js-sdk-enums';
import {BucketingEvent} from './BucketingEvent';
import {ConversionEvent} from './ConversionEvent';
export type VisitorEvent = {
  eventType: EventType;
  data?: BucketingEvent | ConversionEvent;
};
