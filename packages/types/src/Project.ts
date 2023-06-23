/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {Id} from './Id';
import {DoNotTrack, ProjectType} from '@convertcom/enums';
export type Project = {
  id: Id;
  name: string;
  type: ProjectType;
  environments: Record<string, string>;
  utc_offset: number;
  domains: Array<Record<string, any>>;
  // [
  //   {
  //     tld: string,
  //     hosts: null
  //   }
  // ],
  global_javascript?: string;
  settings: {
    auto_link: boolean;
    data_anonymization: boolean;
    do_not_track: DoNotTrack;
    include_jquery: boolean;
    min_order_value?: number;
    max_order_value?: number;
    integrations?: {
      ga?: {
        enabled: boolean;
        property_UA: string;
        auto_revenue_tracking: boolean;
      };
      kissmetrics?: {
        enabled: boolean;
      };
    };
  };
};
