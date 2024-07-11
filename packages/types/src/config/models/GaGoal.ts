/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ConfigGoalBase } from './ConfigGoalBase';
import type { GaGoalSettings } from './GaGoalSettings';

export type GaGoal = (ConfigGoalBase & {
    type?: 'ga_import';
    settings?: GaGoalSettings;
});