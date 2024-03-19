/* generated using openapi-typescript-codegen -- do no edit */
/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
export type SortDirection = {
    /**
     * Data sorting direction using "sort_by" field. "asc" for ascending direction, "desc" for descending direction
     *
     * Defaults to **desc** when not sent in a request
     *
     */
    sort_direction?: SortDirection.sort_direction | null;
};
export namespace SortDirection {
    /**
     * Data sorting direction using "sort_by" field. "asc" for ascending direction, "desc" for descending direction
     *
     * Defaults to **desc** when not sent in a request
     *
     */
    export enum sort_direction {
        ASC = 'asc',
        DESC = 'desc',
    }
}

