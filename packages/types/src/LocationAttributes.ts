/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {IdentityField} from './IndentityField';

export type LocationAttributes = {
  locationProperties?: Record<any, any>;
  identityField?: IdentityField;
  forceEvent?: boolean;
};
