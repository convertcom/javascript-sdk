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
    browser?: 'IE' | 'CH' | 'FF' | 'OP' | 'SF' | 'OTH';
    /**
     * List of device classes that the visitor device falls into
     */
    devices?: Array<'ALLPH' | 'IPH' | 'OTHPH' | 'ALLTAB' | 'IPAD' | 'OTHTAB' | 'DESK' | 'OTHDEV'>;
    /**
     * Traffic source
     */
    source?: 'campaign' | 'search' | 'referral' | 'direct';
    /**
     * Campaign string
     */
    campaign?: string;
    /**
     * Type of the visitor
     */
    visitorType?: 'new' | 'returning';
    /**
     * Two ISO country code for visitor's country
     */
    country?: string;
    /**
     * Custom Segments as defined inside Convert app. This will be the list of segments' IDs
     */
    customSegments?: Array<string>;
}