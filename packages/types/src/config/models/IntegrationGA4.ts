/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { IntegrationGA4Base } from './IntegrationGA4Base';

export type IntegrationGA4 = (IntegrationGA4Base & {
    /**
     * ID of the ga4 property where data will be sent. Used internally for API calls to GoogleAnalytics
     */
    propertyId?: string;
});