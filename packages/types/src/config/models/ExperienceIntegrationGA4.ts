/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { ExperienceIntegrationGA4Base } from './ExperienceIntegrationGA4Base';
import type { IntegrationGA4 } from './IntegrationGA4';
export type ExperienceIntegrationGA4 = (ExperienceIntegrationGA4Base & IntegrationGA4 & {
    /**
     * List of GA audiences created for each of this experience's variations
     */
    audiences?: Record<string, string>;
});

