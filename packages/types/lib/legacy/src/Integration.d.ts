/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import { GoogleAnalyticsType, IntegrationProvider } from '@convertcom/enums';
export type Integration = {
    provider: IntegrationProvider;
    enabled?: boolean;
    custom_dimension?: string;
    type?: GoogleAnalyticsType;
    measurementId?: string;
    property_UA?: string;
    evar?: string;
};
