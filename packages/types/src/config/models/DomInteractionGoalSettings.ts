/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
export type DomInteractionGoalSettings = {
    /**
     * Array of Events to be tracked by this goal
     */
    tracked_items: Array<{
        /**
         * Css selector that identifies the DOM element(s) on which 'event' is to be monitored in order to fire the goal.
         */
        selector?: string;
        /**
         * The event to monitor in order to fire the goal.
         */
        event?: string;
    }>;
};

