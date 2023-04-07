/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {Id} from './Id';
import {VariableType} from './VariableType';

export type FeatureVariableDefinition = {
  key: string;
  type?: VariableType;
};

export type Feature = {
  id: Id;
  name: string;
  key: string;
  variables: Array<FeatureVariableDefinition>;
};
