/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { BaseRule } from './BaseRule';
export type BaseRuleWithWeatherConditionValue = (BaseRule & {
    /**
     * Weather Condition name used for matching. Full or partial condition.
     * The weather provider used by Convert detects the following conditions:
     * - Blizzard
     * - Blowing snow
     * - Cloudy
     * - Fog
     * - Freezing drizzle
     * - Freezing fog
     * - Heavy freezing drizzle
     * - Heavy rain
     * - Heavy rain at times
     * - Light drizzle
     * - Light freezing rain
     * - Light rain
     * - Mist
     * - Moderate rain
     * - Moderate rain at times
     * - Overcast
     * - Partly cloudy
     * - Patchy freezing drizzle possible
     * - Patchy light drizzle
     * - Patchy light rain
     * - Patchy rain possible
     * - Patchy sleet possible
     * - Patchy snow possible
     * - Sunny
     * - Thundery outbreaks possible
     *
     */
    value?: string;
});

