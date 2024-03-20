/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { ConfigGoalBase } from './ConfigGoalBase';
import type { GaGoalSettings } from './GaGoalSettings';
export type GaGoal = (ConfigGoalBase & {
    type?: GaGoal.type;
    settings?: GaGoalSettings;
});
export namespace GaGoal {
    export enum type {
        GA_IMPORT = 'ga_import',
    }
}

