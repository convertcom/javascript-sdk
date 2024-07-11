/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ClicksLinkGoalSettings } from './ClicksLinkGoalSettings';
import type { ConfigGoalBase } from './ConfigGoalBase';

export type ClicksLinkGoal = (ConfigGoalBase & {
    type?: 'clicks_link';
    settings?: ClicksLinkGoalSettings;
});