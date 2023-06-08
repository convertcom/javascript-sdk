/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {
  BrowserType,
  DeviceType,
  SegmentsKeys,
  SourceType,
  VisitorType
} from '@convertcom/enums';

export type SegmentsData = {
  [SegmentsKeys.COUNTRY]?: string;
  [SegmentsKeys.BROWSER]?: BrowserType;
  [SegmentsKeys.DEVICES]?: Array<DeviceType>;
  [SegmentsKeys.SOURCE]?: SourceType;
  [SegmentsKeys.CAMPAIGN]?: string;
  [SegmentsKeys.VISITOR_TYPE]?: VisitorType;
  [SegmentsKeys.CUSTOM_SEGMENTS]?: Array<string>;
};
