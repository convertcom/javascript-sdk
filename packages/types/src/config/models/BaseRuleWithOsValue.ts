/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import type { BaseRule } from './BaseRule';
export type BaseRuleWithOsValue = (BaseRule & {
    /**
     * Operating System name used for matching
     */
    value?: BaseRuleWithOsValue.value;
});
export namespace BaseRuleWithOsValue {
    /**
     * Operating System name used for matching
     */
    export enum value {
        ANDROID = 'android',
        IPHONE = 'iphone',
        IPOD = 'ipod',
        IPAD = 'ipad',
        WINDOWS = 'windows',
        MACOS = 'macos',
        LINUX = 'linux',
    }
}

