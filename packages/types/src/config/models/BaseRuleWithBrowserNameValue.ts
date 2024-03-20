/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { BaseRule } from './BaseRule';
export type BaseRuleWithBrowserNameValue = (BaseRule & {
    /**
     * Browser name used for matching
     */
    value?: BaseRuleWithBrowserNameValue.value;
});
export namespace BaseRuleWithBrowserNameValue {
    /**
     * Browser name used for matching
     */
    export enum value {
        CHROME = 'chrome',
        MICROSOFT_IE = 'microsoft_ie',
        FIREFOX = 'firefox',
        MICROSOFT_EDGE = 'microsoft_edge',
        MOZILLA = 'mozilla',
        OPERA = 'opera',
        SAFARI = 'safari',
    }
}

