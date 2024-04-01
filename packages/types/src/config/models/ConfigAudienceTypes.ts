/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
/**
 * Type of the Audience. Can be one of the following: `permanent`, `transient`. For full-stack projects, `transient` is the only valid option, the rest will be ignored.
 * * **permanent** - A permanent audience is one that is checked only at the time the user is being bucketed into the experience
 * * **transient** - A transient audience is one that is checked every time the user is being bucketed into the experience
 *
 */
export enum ConfigAudienceTypes {
    PERMANENT = 'permanent',
    TRANSIENT = 'transient',
}
