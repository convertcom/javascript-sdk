/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

/**
 * CoreDecider interface for type checking across packages
 * The actual implementation lives in @convertcom/js-sdk-data
 */
export interface CoreDecider {
  initialize(): Promise<void>;
  isReady(): boolean;
}
