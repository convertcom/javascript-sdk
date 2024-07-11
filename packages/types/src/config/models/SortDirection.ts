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
    sort_direction?: 'asc' | 'desc' | null;
}