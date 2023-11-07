/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */
export enum GoalType {
  ADVANCED = 'advanced',
  DOM_INTERACTION = 'dom_interaction',
  SCROLL_PERCENTAGE = 'scroll_percentage',
  CODE_TRIGGER = 'code_trigger',
  REVENUE = 'revenue',
  GOOGLE_ANALYTICS_IMPORT = 'ga_import',
  CLICKS_ELEMENT = 'clicks_element',
  CLICKS_LINK = 'clicks_link',
  SUBMITS_FORM = 'submits_form',
  VISITS_PAGE = 'visits_page'
}

export enum GoalRevenueTriggeringType {
  MANUAL = 'manual',
  GOOGLE_ANALYTICS = 'ga'
}
