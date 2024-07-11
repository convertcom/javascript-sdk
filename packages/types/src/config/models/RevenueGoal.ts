/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ConfigGoalBase } from './ConfigGoalBase';
import type { RevenueGoalSettings } from './RevenueGoalSettings';

export type RevenueGoal = (ConfigGoalBase & {
    type?: 'revenue';
    settings?: RevenueGoalSettings;
});