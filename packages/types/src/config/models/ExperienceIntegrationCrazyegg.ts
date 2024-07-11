/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ExperienceIntegrationBase } from './ExperienceIntegrationBase';

/**
 * Crazyegg integration requires API key and secret which are set at the project level and can be
 * updated using updateProject operation
 *
 * **Important:** Not having API key and secret set for the project, would cause integration to fail
 *
 */
export type ExperienceIntegrationCrazyegg = ExperienceIntegrationBase;