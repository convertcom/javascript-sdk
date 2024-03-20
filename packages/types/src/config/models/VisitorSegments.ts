/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
/**
 * Segments under which this visitor is placed. Some defaults keys are accepted and any other free field **key value** could be used
 * for fullstack projects
 *
 */
export type VisitorSegments = {
    /**
     * Browser used:
     * IE - Internet Explorer
     * CH - Chrome
     * FF - Firefox
     * OP - Opera
     * SF - Safari
     * OTH - Other
     *
     */
    browser?: VisitorSegments.browser;
    /**
     * List of device classes that the visitor device falls into
     */
    devices?: Array<'ALLPH' | 'IPH' | 'OTHPH' | 'ALLTAB' | 'IPAD' | 'OTHTAB' | 'DESK' | 'OTHDEV'>;
    /**
     * Traffic source
     */
    source?: VisitorSegments.source;
    /**
     * Campaign string
     */
    campaign?: string;
    /**
     * Type of the visitor
     */
    visitorType?: VisitorSegments.visitorType;
    /**
     * Two ISO country code for visitor's country
     */
    country?: string;
    /**
     * Custom Segments as defined inside Convert app. This will be the list of segments' IDs
     */
    customSegments?: Array<string>;
};
export namespace VisitorSegments {
    /**
     * Browser used:
     * IE - Internet Explorer
     * CH - Chrome
     * FF - Firefox
     * OP - Opera
     * SF - Safari
     * OTH - Other
     *
     */
    export enum browser {
        IE = 'IE',
        CH = 'CH',
        FF = 'FF',
        OP = 'OP',
        SF = 'SF',
        OTH = 'OTH',
    }
    /**
     * Traffic source
     */
    export enum source {
        CAMPAIGN = 'campaign',
        SEARCH = 'search',
        REFERRAL = 'referral',
        DIRECT = 'direct',
    }
    /**
     * Type of the visitor
     */
    export enum visitorType {
        NEW = 'new',
        RETURNING = 'returning',
    }
}

