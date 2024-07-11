/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

export type IntegrationGA4Base = {
    type?: 'ga4';
    /**
     * ID of the ga4 property where data will be sent. Used internally for API calls to GoogleAnalytics
     */
    measurementId?: string;
}