/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ConfigGoalBase } from './ConfigGoalBase';
import type { SubmitsFormGoalSettings } from './SubmitsFormGoalSettings';

export type SubmitsFormGoal = (ConfigGoalBase & {
    type?: 'submits_form';
    settings?: SubmitsFormGoalSettings;
});