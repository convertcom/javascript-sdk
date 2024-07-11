/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

/**
 * This one describes a logical triggering rule that is being used inside the app
 */
export type LocationTriggerBase = {
    type: 'url_change' | 'manual' | 'dom_change' | 'callback';
}