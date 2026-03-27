/*
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {RuleElement} from './config/index';

export type RuleDataValue =
  | string
  | number
  | boolean
  | Record<string, any>
  | Array<any>
  | null
  | undefined;

export type RuleDataMethod<T = RuleDataValue> = (rule?: RuleElement) => T;

export type RuleData = {
  name: 'RuleData';
  get?: (method?: RuleElement | string) => Record<string, any>;
  getUrl?: RuleDataMethod<string>;
  getUrlWithQuery?: RuleDataMethod<string>;
  getQueryString?: RuleDataMethod<string>;
  getPageTagPageType?: RuleDataMethod<string | undefined>;
  getPageTagCategoryId?: RuleDataMethod<string | undefined>;
  getPageTagCategoryName?: RuleDataMethod<string | undefined>;
  getPageTagProductSku?: RuleDataMethod<string | undefined>;
  getPageTagProductName?: RuleDataMethod<string | undefined>;
  getPageTagProductPrice?: RuleDataMethod<string | undefined>;
  getPageTagCustomerId?: RuleDataMethod<string | undefined>;
  getPageTagCustom1?: RuleDataMethod<string | undefined>;
  getPageTagCustom2?: RuleDataMethod<string | undefined>;
  getPageTagCustom3?: RuleDataMethod<string | undefined>;
  getPageTagCustom4?: RuleDataMethod<string | undefined>;
  getWeatherCondition?: RuleDataMethod<string | undefined>;
  getJsCondition?: RuleDataMethod<boolean>;
  getIsDesktop?: RuleDataMethod<boolean>;
  getIsMobile?: RuleDataMethod<boolean>;
  getIsTablet?: RuleDataMethod<boolean>;
  getUserAgent?: RuleDataMethod<string>;
  getOs?: RuleDataMethod<string>;
  getBrowserVersion?: RuleDataMethod<string>;
  getBrowserName?: RuleDataMethod<string>;
  getProjectTimeMinuteOfHour?: RuleDataMethod<number>;
  getProjectTimeHourOfDay?: RuleDataMethod<number>;
  getProjectTimeDayOfWeek?: RuleDataMethod<number>;
  getLocalTimeMinuteOfHour?: RuleDataMethod<number>;
  getLocalTimeHourOfDay?: RuleDataMethod<number>;
  getLocalTimeDayOfWeek?: RuleDataMethod<number>;
  getBucketedIntoSegment?: RuleDataMethod<Record<string, any>>;
  getBucketedIntoExperience?: RuleDataMethod<boolean>;
  getVisitsCount?: RuleDataMethod<number>;
  getVisitorType?: RuleDataMethod<string>;
  getVisitorId?: RuleDataMethod<string | boolean>;
  getVisitorDataExists?: RuleDataMethod<boolean>;
  getCookie?: RuleDataMethod<string | undefined>;
  getVisitDuration?: RuleDataMethod<number>;
  getGoalTriggered?: RuleDataMethod<Record<string, any>>;
  getPagesVisitedCount?: RuleDataMethod<number>;
  getLanguage?: RuleDataMethod<string>;
  getDaysSinceLastVisit?: RuleDataMethod<number>;
  getRegion?: RuleDataMethod<string | undefined>;
  getCountry?: RuleDataMethod<string | undefined>;
  getCity?: RuleDataMethod<string | undefined>;
  getAvgTimePage?: RuleDataMethod<number>;
  getSourceName?: RuleDataMethod<string>;
  getMedium?: RuleDataMethod<string>;
  getKeyword?: RuleDataMethod<string>;
  getCampaign?: RuleDataMethod<string>;
  [key: string]: any;
};
