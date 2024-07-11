/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { IntegrationGA4Base } from './IntegrationGA4Base';
import type { ProjectGASettingsBase } from './ProjectGASettingsBase';

export type ProjectIntegrationGA4 = (ProjectGASettingsBase & IntegrationGA4Base & {
    /**
     * Boolean indicating whether to wait for the page view event to complete before sending other events.
     */
    no_wait_pageview?: boolean;
});