/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ExperienceIntegrationBase } from './ExperienceIntegrationBase';

export type ExperienceIntegrationBaidu = (ExperienceIntegrationBase & {
    /**
     * Custom dimension where experience data should be sent to.
     */
    custom_dimension: string;
});