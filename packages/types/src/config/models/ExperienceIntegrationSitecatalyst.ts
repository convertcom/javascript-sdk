/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ExperienceIntegrationBase } from './ExperienceIntegrationBase';

export type ExperienceIntegrationSitecatalyst = (ExperienceIntegrationBase & {
    /**
     * Custom dimension where experience data should be sent to.
     */
    evar: string;
});