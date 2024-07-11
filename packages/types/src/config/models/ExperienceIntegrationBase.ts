/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { IntegrationProvider } from './IntegrationProvider';

export type ExperienceIntegrationBase = {
    provider: IntegrationProvider;
    /**
     * Boolean flag indicating whether the integration is enabled or not. When updating experience's integrations,
     * to disable an integration, this flag needs to be passed as **false**. If not passed, integration is assumed to be **enabled=true**
     *
     */
    enabled?: boolean | null;
}