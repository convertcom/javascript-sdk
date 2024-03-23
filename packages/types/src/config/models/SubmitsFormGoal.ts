/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { ConfigGoalBase } from './ConfigGoalBase';
import type { SubmitsFormGoalSettings } from './SubmitsFormGoalSettings';
export type SubmitsFormGoal = (ConfigGoalBase & {
    type?: SubmitsFormGoal.type;
    settings?: SubmitsFormGoalSettings;
});
export namespace SubmitsFormGoal {
    export enum type {
        SUBMITS_FORM = 'submits_form',
    }
}

