/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import { GoogleAnalyticsType } from '@convertcom/enums';
export type Integration = {
    provider: string;
    enabled?: boolean;
    custom_dimension?: string;
    ga_type?: GoogleAnalyticsType;
    evar?: string;
};
