/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

import {RuleElement} from './config/index';

/* Example
{
  "OR": [
  {
    "AND": [
      {
        "OR_WHEN": [
          {
            "rule_type": "cookie",
            "matching": {
              "match_type": "matches",
              "negated": false
            },
            "value": "value1",
            "key": "varName1"
          }
        ]
      },
      {
        "OR_WHEN": [
          {
            "rule_type": "cookie",
            "matching": {
              "match_type": "matches",
              "negated": false
            },
            "value": "value2",
            "key": "varName2"
          }
        ]
      }
    ]
  },
  {
    "AND": [
      {
        "OR_WHEN": [
          {
            "rule_type": "cookie",
            "matching": {
              "match_type": "contains",
              "negated": false
            },
            "value": "something",
            "key": "varName3"
          }
        ]
      }
    ]
  }
]
}
*/

export type RuleOrWhen = {
  OR_WHEN: Array<RuleElement>;
};

export type RuleAnd = {
  AND: Array<RuleOrWhen>;
};
