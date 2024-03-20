/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithGoalTriggeredValue } from './BaseRuleWithGoalTriggeredValue';
import type { ChoiceMatchingOptions } from './ChoiceMatchingOptions';
import type { GoalTriggeredMatchRulesTypes } from './GoalTriggeredMatchRulesTypes';
export type GoalTriggeredMatchRule = (BaseRuleWithGoalTriggeredValue & {
    rule_type: GoalTriggeredMatchRulesTypes;
    matching?: (BaseMatch & {
        match_type?: ChoiceMatchingOptions;
    });
});

