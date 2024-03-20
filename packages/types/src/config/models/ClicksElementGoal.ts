/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { ClicksElementGoalSettings } from './ClicksElementGoalSettings';
import type { ConfigGoalBase } from './ConfigGoalBase';
export type ClicksElementGoal = (ConfigGoalBase & {
    type?: ClicksElementGoal.type;
    settings?: ClicksElementGoalSettings;
});
export namespace ClicksElementGoal {
    export enum type {
        CLICKS_ELEMENT = 'clicks_element',
    }
}

