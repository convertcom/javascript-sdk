/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { ConfigGoalBase } from './ConfigGoalBase';
export type NoSettingsGoal = (ConfigGoalBase & {
    type?: NoSettingsGoal.type;
});
export namespace NoSettingsGoal {
    export enum type {
        ADVANCED = 'advanced',
        VISITS_PAGE = 'visits_page',
        CODE_TRIGGER = 'code_trigger',
    }
}

