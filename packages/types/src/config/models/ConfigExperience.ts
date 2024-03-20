/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { ExperienceIntegrationBaidu } from './ExperienceIntegrationBaidu';
import type { ExperienceIntegrationClicktale } from './ExperienceIntegrationClicktale';
import type { ExperienceIntegrationClicky } from './ExperienceIntegrationClicky';
import type { ExperienceIntegrationCnzz } from './ExperienceIntegrationCnzz';
import type { ExperienceIntegrationCrazyegg } from './ExperienceIntegrationCrazyegg';
import type { ExperienceIntegrationEconda } from './ExperienceIntegrationEconda';
import type { ExperienceIntegrationEulerian } from './ExperienceIntegrationEulerian';
import type { ExperienceIntegrationGAServing } from './ExperienceIntegrationGAServing';
import type { ExperienceIntegrationGosquared } from './ExperienceIntegrationGosquared';
import type { ExperienceIntegrationHeapanalytics } from './ExperienceIntegrationHeapanalytics';
import type { ExperienceIntegrationHotjar } from './ExperienceIntegrationHotjar';
import type { ExperienceIntegrationMixpanel } from './ExperienceIntegrationMixpanel';
import type { ExperienceIntegrationMouseflow } from './ExperienceIntegrationMouseflow';
import type { ExperienceIntegrationPiwik } from './ExperienceIntegrationPiwik';
import type { ExperienceIntegrationSegmentio } from './ExperienceIntegrationSegmentio';
import type { ExperienceIntegrationSitecatalyst } from './ExperienceIntegrationSitecatalyst';
import type { ExperienceIntegrationWoopra } from './ExperienceIntegrationWoopra';
import type { ExperienceIntegrationYsance } from './ExperienceIntegrationYsance';
import type { ExperienceTypes } from './ExperienceTypes';
import type { ExperienceVariationConfig } from './ExperienceVariationConfig';
import type { RuleObject } from './RuleObject';
export type ConfigExperience = {
    /**
     * Experience ID
     */
    id?: string;
    /**
     * Experience Name
     */
    name?: string;
    /**
     * Experience readable key that uniquely identifies this experience
     */
    key?: string;
    /**
     * List of locations IDs on which this experience is presented. Either this or **site_area** is given but should not be both.
     */
    locations?: Array<string> | null;
    /**
     * Rules that define where the experience is gonna run. Either this or **locations** is given but should not be both.
     */
    site_area?: RuleObject | null;
    /**
     * List of audiences IDs to which this experience is presented to
     */
    audiences?: Array<string> | null;
    /**
     * List of goals IDs to which will be tracked for this experience
     */
    goals?: Array<string>;
    /**
     * Global Experience's JavaScript that will run for this experience before its changes are applied
     *
     */
    global_js?: string;
    /**
     * Global Experience's StyleSheet that will run for this experience before its changes are applied
     *
     */
    global_css?: string;
    type?: ExperienceTypes;
    /**
     * Experience's version number
     */
    version?: number;
    /**
     * Experience's variations list
     */
    variations?: Array<ExperienceVariationConfig>;
    /**
     * List of integrations that this experience's data is sent to
     */
    integrations?: Array<(ExperienceIntegrationBaidu | ExperienceIntegrationClicktale | ExperienceIntegrationClicky | ExperienceIntegrationCnzz | ExperienceIntegrationCrazyegg | ExperienceIntegrationEconda | ExperienceIntegrationEulerian | ExperienceIntegrationGAServing | ExperienceIntegrationGosquared | ExperienceIntegrationHeapanalytics | ExperienceIntegrationHotjar | ExperienceIntegrationMixpanel | ExperienceIntegrationMouseflow | ExperienceIntegrationPiwik | ExperienceIntegrationSegmentio | ExperienceIntegrationSitecatalyst | ExperienceIntegrationWoopra | ExperienceIntegrationYsance)>;
    /**
     * List of environments that this experience is supposed to run on. The full list of available environments is defined at
     * project level. If this list is empty, the experience will run on all environments.
     *
     */
    environments?: Array<string>;
    /**
     * Various experience's settings
     */
    settings?: {
        /**
         * Minimum order value for transactions outliers
         */
        min_order_value?: number;
        /**
         * Maximum order value for transactions outliers
         */
        max_order_value?: number;
    };
};

