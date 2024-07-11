/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ClicksElementGoalSettings } from './ClicksElementGoalSettings';
import type { ConfigGoalBase } from './ConfigGoalBase';

export type ClicksElementGoal = (ConfigGoalBase & {
    type?: 'clicks_element';
    settings?: ClicksElementGoalSettings;
});