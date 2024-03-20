/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { ClicksLinkGoalSettings } from './ClicksLinkGoalSettings';
import type { ConfigGoalBase } from './ConfigGoalBase';
export type ClicksLinkGoal = (ConfigGoalBase & {
    type?: ClicksLinkGoal.type;
    settings?: ClicksLinkGoalSettings;
});
export namespace ClicksLinkGoal {
    export enum type {
        CLICKS_LINK = 'clicks_link',
    }
}

