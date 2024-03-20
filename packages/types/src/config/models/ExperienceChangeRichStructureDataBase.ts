/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { ExperienceChangeBase } from './ExperienceChangeBase';
/**
 * Describes structure for "defaultCode" type of experience change
 */
export type ExperienceChangeRichStructureDataBase = (ExperienceChangeBase & {
    type?: ExperienceChangeRichStructureDataBase.type;
    /**
     * Describes structure for "defaultCode" type of experience change
     */
    data?: Record<string, string>;
});
export namespace ExperienceChangeRichStructureDataBase {
    export enum type {
        RICH_STRUCTURE = 'richStructure',
    }
}

