/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
export type IntegrationGA4Base = {
    type?: IntegrationGA4Base.type;
    /**
     * ID of the ga4 property where data will be sent. Used internally for API calls to GoogleAnalytics
     */
    measurementId?: string;
};
export namespace IntegrationGA4Base {
    export enum type {
        GA4 = 'ga4',
    }
}

