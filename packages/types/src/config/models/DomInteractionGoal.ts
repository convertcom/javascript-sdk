/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ConfigGoalBase } from './ConfigGoalBase';
import type { DomInteractionGoalSettings } from './DomInteractionGoalSettings';

export type DomInteractionGoal = (ConfigGoalBase & {
    type?: 'dom_interaction';
    settings?: DomInteractionGoalSettings;
});