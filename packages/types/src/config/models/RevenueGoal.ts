/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { ConfigGoalBase } from './ConfigGoalBase';
import type { RevenueGoalSettings } from './RevenueGoalSettings';
export type RevenueGoal = (ConfigGoalBase & {
    type?: RevenueGoal.type;
    settings?: RevenueGoalSettings;
});
export namespace RevenueGoal {
    export enum type {
        REVENUE = 'revenue',
    }
}

