/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { GenericBoolKeyValueMatchRulesTypes } from './GenericBoolKeyValueMatchRulesTypes';
import type { GenericNumericKeyValueMatchRulesTypes } from './GenericNumericKeyValueMatchRulesTypes';
import type { GenericTextKeyValueMatchRulesTypes } from './GenericTextKeyValueMatchRulesTypes';

export type KeyValueMatchRulesTypes = (GenericTextKeyValueMatchRulesTypes & GenericNumericKeyValueMatchRulesTypes & GenericBoolKeyValueMatchRulesTypes);