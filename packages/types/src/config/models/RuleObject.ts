/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { RuleElement } from './RuleElement';
/**
 * This one describes a logical rule that is being used inside the app for triggering goals, matching audiences etc
 */
export type RuleObject = {
    /**
     * This describes an outer set of blocks which are evaluated using OR's between them
     */
    OR?: Array<{
        /**
         * This describes a colections of logical blocks which are evaluated using AND's between them
         */
        AND?: Array<{
            /**
             * This describes a colections of logical blocks which are evaluated using OR's between them
             */
            OR_WHEN?: Array<RuleElement>;
        }>;
    }>;
};

