/*!
 * Convert JS SDK
 * Version 1.0.0
 * Copyright(c) 2020 Convert Insights, Inc
 * License Apache-2.0
 */

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

export type Rule = {
  rule_type?: string;
  key?: string;
  matching: {
    match_type: string;
    negated?: boolean;
  };
  value: string | number;
};

export type RuleOrWhen = {
  OR_WHEN: Array<Rule>;
};

export type RuleAnd = {
  AND: Array<RuleOrWhen>;
};

export type RuleSet = {
  OR: Array<RuleAnd>;
};
