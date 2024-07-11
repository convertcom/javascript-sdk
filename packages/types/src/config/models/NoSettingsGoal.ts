/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ConfigGoalBase } from './ConfigGoalBase';

export type NoSettingsGoal = (ConfigGoalBase & {
    type?: 'advanced' | 'visits_page' | 'code_trigger';
});