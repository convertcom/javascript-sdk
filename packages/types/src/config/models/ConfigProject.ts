/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { GA_Settings } from './GA_Settings';
import type { UTC_Offset } from './UTC_Offset';
/**
 * Project Object under which experiences would get created
 */
export type ConfigProject = {
    /**
     * Project ID
     */
    id?: string;
    /**
     * Project Name. If **settings.data_anonymization** is turned on, the name will be generated from **id** field
     */
    name?: string;
    /**
     * Value which describes project product type
     */
    type?: ConfigProject.type;
    utc_offset?: UTC_Offset;
    /**
     * List of domains allowed to be tracked under this project
     */
    domains?: Array<{
        /**
         * Top level domain, used for setting cookies where applicable
         */
        tld?: string;
        /**
         * List of host names under **tld** which are allowed to be tracked under this project
         */
        hosts?: any;
    }>;
    /**
     * The global javascript code that will be loaded on all pages where
     * the tracking script is installed, prior do processing any of
     * experiences, goals, audiences etc.
     *
     */
    global_javascript?: string | null;
    /**
     * Various project's settings
     */
    settings?: {
        /**
         * Flag indicating whether decoration of outgoing links (appending tracking cookies inside the link URL in order to
         * make cross domain tracking possible) is done automatically on page
         *
         */
        allow_crossdomain_tracking?: boolean;
        /**
         * Whether or not data is [anonymized](https://convert.zendesk.com/hc/en-us/articles/204506339-Prevent-Experiment-Details-Data-Leak-with-Data-Anonymization).
         */
        data_anonymization?: boolean;
        /**
         * Follow the 'Do not track' browser settings for users in the mentioned area of the world.
         */
        do_not_track?: ConfigProject.do_not_track;
        /**
         * Whether to include jQuery library or not into the javascript tracking file served by Convert and loaded via the tracking snippet. If jQuery is not included, it has to be loaded on page, before Convert's tracking code
         */
        include_jquery?: boolean;
        /**
         * Whether to include jQuery library or not into the v1 javascript tracking file served by Convert and loaded via the tracking snippet.
         */
        include_jquery_v1?: boolean;
        /**
         * Whether to disable the SPA (Single Page Application) related functionalities from the tracking scripts V1. Most websites work fine without disabling SPA functionality regardless of the fact they are Single Page Apps or not. In edge situation, this setting might prove handy
         */
        disable_spa_functionality?: boolean;
        /**
         * When this is turned to true, Convert won't track any referral data like http referral, utm query strings etc. Those will be used on the current page if available but won't be stored in cookies in order to be used on subsequent pages.
         */
        do_not_track_referral?: boolean;
        /**
         * This holds project wide settings used by integrations
         */
        integrations?: {
            google_analytics?: GA_Settings;
            kissmetrics?: {
                /**
                 * Flag indicating whether Kissmetrics integration is enabled or not for this project
                 */
                enabled?: boolean;
            };
            shopify?: {
                /**
                 * ID of the selected goal at Convert Shopify App
                 */
                revenueGoalId?: string;
            };
        };
        /**
         * Minimum order value for transactions outliers
         */
        min_order_value?: number;
        /**
         * Maximum order value for transactions outliers
         */
        max_order_value?: number;
    };
    /**
     * A user-defined key-value object which describes environments available for the project.
     */
    environments?: Record<string, any>;
};
export namespace ConfigProject {
    /**
     * Value which describes project product type
     */
    export enum type {
        FULLSTACK = 'fullstack',
        WEB = 'web',
    }
    /**
     * Follow the 'Do not track' browser settings for users in the mentioned area of the world.
     */
    export enum do_not_track {
        OFF = 'OFF',
        EU_ONLY = 'EU ONLY',
        EEA_ONLY = 'EEA ONLY',
        WORLDWIDE = 'Worldwide',
    }
}

