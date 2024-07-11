/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

export type Pagination = {
    /**
     * Current page number
     */
    current_page?: number;
    /**
     * Total number of records
     */
    items_count?: number;
    /**
     * Number of records per page
     */
    items_per_page?: number;
    /**
     * Limitation number of records per page
     */
    pages_count?: number;
}