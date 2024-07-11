/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

export type RevenueGoalSettings = {
    /**
     * Type of the revenue goal tracking, one of the below.
     * * "manual" - goal will be triggered through the given revenue tracking code;
     * An empty **triggering_rule** has to be provided as that takes priority over manual triggering
     * * "ga" - Convert will attempt to pick revenue from GA revenue tracking code and attach it to this goal,
     * when on page where this goal is triggered via "triggering_rule"
     *
     */
    triggering_type: 'manual' | 'ga';
}