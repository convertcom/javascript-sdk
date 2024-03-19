/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { ConfigGoalBase } from './ConfigGoalBase';
import type { DomInteractionGoalSettings } from './DomInteractionGoalSettings';
export type DomInteractionGoal = (ConfigGoalBase & {
    type?: DomInteractionGoal.type;
    settings?: DomInteractionGoalSettings;
});
export namespace DomInteractionGoal {
    export enum type {
        DOM_INTERACTION = 'dom_interaction',
    }
}

