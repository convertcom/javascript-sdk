/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

export type BaseMatch = {
    /**
     * When true, the rule result is gonna be negated.
     * example: `url contains "test"` with *negated* = true becomes `url does not contain "test"`
     *
     */
    negated?: boolean;
}