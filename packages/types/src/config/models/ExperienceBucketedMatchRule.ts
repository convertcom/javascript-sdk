/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { BaseMatch } from './BaseMatch';
import type { BaseRuleWithExperienceBucketedValue } from './BaseRuleWithExperienceBucketedValue';
import type { ChoiceMatchingOptions } from './ChoiceMatchingOptions';
export type ExperienceBucketedMatchRule = (BaseRuleWithExperienceBucketedValue & {
    rule_type: string;
    matching?: (BaseMatch & {
        match_type?: ChoiceMatchingOptions;
    });
});

