/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ConfigAudience } from './ConfigAudience';
import type { ConfigExperience } from './ConfigExperience';
import type { ConfigFeature } from './ConfigFeature';
import type { ConfigGoal } from './ConfigGoal';
import type { ConfigLocation } from './ConfigLocation';
import type { ConfigProject } from './ConfigProject';
import type { ConfigSegment } from './ConfigSegment';

/**
 * Response containing project's config data needed in order to serve experiences
 */
export type ConfigResponseData = {
    /**
     * Account ID under which the project exists
     */
    account_id?: string;
    project?: ConfigProject;
    /**
     * List of goals to be tracked in the project
     */
    goals?: Array<ConfigGoal>;
    /**
     * List of locations that are used inside this project
     */
    locations?: Array<ConfigLocation>;
    /**
     * List of audiences that are used inside this project
     */
    audiences?: Array<ConfigAudience>;
    /**
     * List of custom that are devined inside this project
     */
    segments?: Array<ConfigSegment>;
    /**
     * List of active experiences inside this project
     */
    experiences?: Array<ConfigExperience>;
    /**
     * List of archived experiences inside this project, which were archived within the last 8 months
     */
    archived_experiences?: Array<string>;
    /**
     * List of features inside this project. Presented only for fullstack projects
     */
    features?: Array<ConfigFeature>;
}