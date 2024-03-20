/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { BaseRule } from './BaseRule';
export type BaseRuleWithVisitorTypeValue = (BaseRule & {
    /**
     * Type of the visitors
     */
    value?: BaseRuleWithVisitorTypeValue.value;
});
export namespace BaseRuleWithVisitorTypeValue {
    /**
     * Type of the visitors
     */
    export enum value {
        NEW = 'new',
        RETURNING = 'returning',
    }
}

