/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { GA_SettingsBase } from './GA_SettingsBase';

export type ProjectGASettingsBase = (GA_SettingsBase & {
    /**
     * Attempt to pull revenue data from Google Analytics Revenue Tracking code.
     */
    auto_revenue_tracking?: boolean;
});