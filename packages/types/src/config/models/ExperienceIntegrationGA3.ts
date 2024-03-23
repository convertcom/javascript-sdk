/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { ExperienceIntegrationBase } from './ExperienceIntegrationBase';
import type { GA_SettingsBase } from './GA_SettingsBase';
import type { IntegrationGA3 } from './IntegrationGA3';
export type ExperienceIntegrationGA3 = (GA_SettingsBase & ExperienceIntegrationBase & IntegrationGA3 & {
    /**
     * Custom dimension where experience data should be sent to.
     */
    custom_dimension?: string;
});

