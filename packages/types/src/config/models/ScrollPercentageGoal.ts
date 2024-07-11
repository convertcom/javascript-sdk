/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ConfigGoalBase } from './ConfigGoalBase';
import type { ScrollPercentageGoalSettings } from './ScrollPercentageGoalSettings';

export type ScrollPercentageGoal = (ConfigGoalBase & {
    type?: 'scroll_percentage';
    settings?: ScrollPercentageGoalSettings;
});