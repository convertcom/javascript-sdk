/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
import {BrowserType} from '../enums/segments/browser-type';
import {DeviceType} from '../enums/segments/device-type';
import {SegmentsKeys} from '../enums/segments/segments-keys';
import {SourceType} from '../enums/segments/source-type';
import {VisitorType} from '../enums/segments/visitor-type';
export type SegmentsData = {
  [SegmentsKeys.BROWSER]?: BrowserType;
  [SegmentsKeys.DEVICES]?: Array<DeviceType>;
  [SegmentsKeys.SOURCE]?: SourceType;
  [SegmentsKeys.CAMPAIGN]?: string;
  [SegmentsKeys.VISITOR_TYPE]?: VisitorType;
  [SegmentsKeys.CUSTOM_SEGMENTS]?: Array<string>;
};
