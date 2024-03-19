/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { ConfigGoalBase } from './ConfigGoalBase';
import type { ScrollPercentageGoalSettings } from './ScrollPercentageGoalSettings';
export type ScrollPercentageGoal = (ConfigGoalBase & {
    type?: ScrollPercentageGoal.type;
    settings?: ScrollPercentageGoalSettings;
});
export namespace ScrollPercentageGoal {
    export enum type {
        SCROLL_PERCENTAGE = 'scroll_percentage',
    }
}

