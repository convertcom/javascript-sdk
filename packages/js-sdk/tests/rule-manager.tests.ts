import 'mocha';
import {expect} from 'chai';
import {assert} from 'chai';

import {RuleManager as rm} from '@convertcom/rules';
import {Comparisons as comparisonProcessor} from '@convertcom/utils';
import testConfig from './test-config.json';
import {RuleSet, Config} from '@convertcom/types';

describe('RuleManager tests', function () {
  it('Should expose RuleManager', function () {
    assert.isDefined(rm);
  });
  it('Imported entity should be a constructor of RuleManager instance', function () {
    expect(rm)
      .to.be.a('function')
      .that.has.property('name')
      .which.equal('RuleManager');
  });

  describe('RuleManager with custom comparison processor', function () {
    let ruleManager;
    const customComparisonProcessor = {
      isTypeOf: function (
        value: string | number,
        testAgainst: string,
        negation?: boolean
      ): boolean {
        if (negation) {
          return typeof value !== testAgainst;
        }
        return typeof value === testAgainst;
      }
    };

    // eslint-disable-next-line mocha/no-setup-in-describe
    const configuration = {
      ...testConfig,
      rules: {
        comparisonProcessor: customComparisonProcessor,
        keys_case_sensitive: false
      }
    } as unknown as Config;
    const testRuleSet1: RuleSet = {
      OR: [
        {
          AND: [
            {
              OR_WHEN: [
                {
                  key: 'sum',
                  matching: {
                    match_type: 'isTypeOf',
                    negated: false
                  },
                  value: 'number'
                }
              ]
            }
          ]
        }
      ]
    };
    const testRuleSet2: RuleSet = {
      OR: [
        {
          AND: [
            {
              OR_WHEN: [
                {
                  key: 'sum',
                  matching: {
                    match_type: 'isTypeOf',
                    negated: true
                  },
                  value: 'number'
                }
              ]
            }
          ]
        }
      ]
    };
    const testRuleSet3: RuleSet = {
      OR: [
        {
          AND: [
            {
              OR_WHEN: [
                {
                  key: 'SUM',
                  matching: {
                    match_type: 'isTypeOf',
                    negated: false
                  },
                  value: 'number'
                }
              ]
            }
          ]
        }
      ]
    };
    const data1 = {
      sum: 'not a number'
    };
    const data2 = {
      sum: 44
    };
    it('Should successfully create a RuleManager instance with default config', function () {
      const ruleManager = new rm();
      expect(ruleManager)
        .to.be.an('object')
        .that.has.property('constructor')
        .that.has.property('name')
        .which.equal('RuleManager');
    });
    it('Should create a RuleManager instance', function () {
      ruleManager = new rm(configuration);
      expect(ruleManager)
        .to.be.an('object')
        .that.has.property('constructor')
        .that.has.property('name')
        .which.equal('RuleManager');
    });
    it('Should has custom Comparison processor', function () {
      expect(ruleManager)
        .to.has.property('comparisonProcessor')
        .which.is.an('object');
    });
    it('getComparisonProcessorMethods should return comparisons list which is equal Custom Comparison processor list', function () {
      expect(ruleManager.getComparisonProcessorMethods()).to.have.deep.members(
        Object.getOwnPropertyNames(customComparisonProcessor).filter(
          (name) => typeof customComparisonProcessor[name] === 'function'
        )
      );
    });
    it(
      'isRuleMatched should return false checking data ' +
        JSON.stringify(data1) +
        ' against rule ' +
        JSON.stringify(testRuleSet1),
      function () {
        expect(ruleManager.isRuleMatched(data1, testRuleSet1)).to.equal(false);
      }
    );
    it(
      'isRuleMatched should return true checking data ' +
        JSON.stringify(data2) +
        ' against rule ' +
        JSON.stringify(testRuleSet1),
      function () {
        expect(ruleManager.isRuleMatched(data2, testRuleSet1)).to.equal(true);
      }
    );
    it(
      'isRuleMatched should return false checking data ' +
        JSON.stringify(data1) +
        ' against rule ' +
        JSON.stringify(testRuleSet2) +
        ' with custom negation',
      function () {
        expect(ruleManager.isRuleMatched(data1, testRuleSet2)).to.equal(true);
      }
    );
    it(
      'isRuleMatched should return false checking data ' +
        JSON.stringify(data2) +
        ' against rule ' +
        JSON.stringify(testRuleSet2) +
        ' with custom negation',
      function () {
        expect(ruleManager.isRuleMatched(data2, testRuleSet2)).to.equal(false);
      }
    );
    it(
      'isRuleMatched should return true checking data ' +
        JSON.stringify(data2) +
        ' against rule ' +
        JSON.stringify(testRuleSet3) +
        ' because of case insensitive for keys',
      function () {
        expect(ruleManager.isRuleMatched(data2, testRuleSet2)).to.equal(false);
      }
    );
  });

  describe('RuleManager with default comparison processor', function () {
    let ruleManager;
    // eslint-disable-next-line mocha/no-setup-in-describe
    const configuration = {
      ...testConfig
    } as unknown as Config;
    const testRuleSet1: RuleSet = {
      OR: [
        {
          AND: [
            {
              OR_WHEN: [
                {
                  key: 'device',
                  matching: {
                    match_type: 'equals',
                    negated: false
                  },
                  value: 'pc'
                },
                {
                  key: 'price',
                  matching: {
                    match_type: 'less',
                    negated: false
                  },
                  value: 100
                }
              ]
            }
          ]
        }
      ]
    };
    const testRuleSet2: RuleSet = {
      OR: [
        {
          AND: [
            {
              OR_WHEN: [
                {
                  key: 'device',
                  matching: {
                    match_type: 'equals',
                    negated: true
                  },
                  value: 'pc'
                },
                {
                  key: 'device',
                  matching: {
                    match_type: 'isIn',
                    negated: false
                  },
                  value: 'phone|tablet'
                }
              ]
            }
          ]
        }
      ]
    };
    const testRuleSet3: RuleSet = {
      OR: [
        {
          AND: [
            {
              OR_WHEN: [
                {
                  key: 'device',
                  matching: {
                    match_type: 'isIn',
                    negated: false
                  },
                  value: 'phone|tablet'
                }
              ]
            },
            {
              OR_WHEN: [
                {
                  key: 'browser',
                  matching: {
                    match_type: 'equals',
                    negated: false
                  },
                  value: 'Mozilla'
                }
              ]
            }
          ]
        },
        {
          AND: [
            {
              OR_WHEN: [
                {
                  key: 'age',
                  matching: {
                    match_type: 'less',
                    negated: true
                  },
                  value: 30
                }
              ]
            }
          ]
        }
      ]
    };
    const testRuleSetWrong = [
      [
        {
          key: 'device',
          matching: {
            match_type: 'equals',
            negated: false
          },
          data: 'pc'
        }
      ]
    ];
    const testRuleSetWrong2 = {
      OR: [
        {
          AND: [
            {
              OR_WHEN: [
                {
                  key: 'device',
                  matching: {
                    match_type: 'equals',
                    negated: false
                  },
                  data: 'pc'
                }
              ]
            },
            [
              {
                key: 'age',
                matching: {
                  match_type: 'less',
                  negated: true
                },
                data: 30
              }
            ]
          ]
        }
      ]
    };

    const data1 = {
      device: 'pc',
      browser: 'Mozilla',
      price: 3
    };
    const data12 = {
      device: 'tablet',
      browser: 'Mozilla',
      price: 3
    };
    const data13 = {
      DEVICE: 'tablet',
      BROWSER: 'Mozilla',
      PRICE: 3
    };
    const data2 = {
      browser: 'Chrome'
    };
    const data21 = 'phone';
    const data22 = {
      device: 'phone'
    };
    const data31 = {
      device: 'tablet',
      browser: 'Mozilla',
      age: 10
    };
    const data32 = {
      device: 'pc',
      browser: 'Chrome',
      age: 31
    };
    it('Should create a RuleManager instance', function () {
      ruleManager = new rm(configuration);
      expect(ruleManager)
        .to.be.an('object')
        .that.has.property('constructor')
        .that.has.property('name')
        .which.equal('RuleManager');
    });
    it('isValidRule should return true', function () {
      expect(
        ruleManager.isValidRule({
          key: 'device',
          matching: {
            match_type: 'contains',
            negated: false
          },
          value: 'phone'
        })
      ).to.equal(true);
    });
    it('isValidRule should return false (bad structure)', function () {
      expect(
        ruleManager.isValidRule({
          matching: 'contains',
          data: 'phone'
        })
      ).to.equal(false);
    });
    it('isValidRule should return false (no `matching` field)', function () {
      expect(
        ruleManager.isValidRule({
          key: 'device',
          value: 'phone'
        })
      ).to.equal(false);
    });
    it('isValidRule should return false (no `value` field)', function () {
      expect(
        ruleManager.isValidRule({
          key: 'device',
          matching: {
            match_type: 'contains',
            negated: false
          }
        })
      ).to.equal(false);
    });
    it('Should has default Comparison processor instance because external is not provided', function () {
      expect(ruleManager)
        .to.has.property('comparisonProcessor')
        .which.has.property('name')
        .which.equal('Comparisons');
    });
    it('getComparisonProcessorMethods should return comparisons list which is equal default Comparison processor list', function () {
      expect(ruleManager.getComparisonProcessorMethods()).to.have.deep.members(
        Object.getOwnPropertyNames(comparisonProcessor).filter(
          (name) => typeof comparisonProcessor[name] === 'function'
        )
      );
    });
    it(
      'isRuleMatched should return true checking data ' +
        JSON.stringify(data1) +
        ' against rule ' +
        JSON.stringify(testRuleSet1),
      function () {
        expect(ruleManager.isRuleMatched(data1, testRuleSet1)).to.equal(true);
      }
    );
    it(
      'isRuleMatched should return true checking data ' +
        JSON.stringify(data12) +
        ' against rule ' +
        JSON.stringify(testRuleSet1),
      function () {
        expect(ruleManager.isRuleMatched(data12, testRuleSet1)).to.equal(true);
      }
    );
    it(
      'isRuleMatched should return false checking data ' +
        JSON.stringify(data13) +
        ' against rule ' +
        JSON.stringify(testRuleSet1) +
        ' baceuse of case sensitive for keys',
      function () {
        expect(ruleManager.isRuleMatched(data13, testRuleSet1)).to.equal(false);
      }
    );
    it(
      'isRuleMatched should return false checking data ' +
        JSON.stringify(data1) +
        ' against rule ' +
        JSON.stringify(testRuleSet2),
      function () {
        expect(ruleManager.isRuleMatched(data1, testRuleSet2)).to.equal(false);
      }
    );
    it(
      'isRuleMatched should return true checking data ' +
        JSON.stringify(data22) +
        ' against rule ' +
        JSON.stringify(testRuleSet2),
      function () {
        expect(ruleManager.isRuleMatched(data22, testRuleSet2)).to.equal(true);
      }
    );
    it(
      'isRuleMatched should return false checking single string value "' +
        data21 +
        '" against rule ' +
        JSON.stringify(testRuleSet2),
      function () {
        expect(ruleManager.isRuleMatched(data21, testRuleSet2)).to.equal(false);
      }
    );
    it(
      'isRuleMatched should return false checking data ' +
        JSON.stringify(data2) +
        ' against rule ' +
        JSON.stringify(testRuleSetWrong) +
        ' because of wrong rules structure',
      function () {
        expect(ruleManager.isRuleMatched(data2, testRuleSetWrong)).to.equal(
          false
        );
      }
    );
    it(
      'isRuleMatched should return false checking data ' +
        JSON.stringify(data2) +
        ' against rule ' +
        JSON.stringify(testRuleSetWrong2) +
        ' because of wrong rules structure',
      function () {
        expect(ruleManager.isRuleMatched(data2, testRuleSetWrong2)).to.equal(
          false
        );
      }
    );
    it('isRuleMatched should return just false because empty data provided', function () {
      expect(ruleManager.isRuleMatched([], {})).to.equal(false);
    });
    it('isRuleMatched should return just false because wrong data provided', function () {
      expect(ruleManager.isRuleMatched('a string', 1234567)).to.equal(false);
    });
    it('isRuleMatched should return just false because wrong rule record provided', function () {
      expect(ruleManager.isRuleMatched({}, [[['a string']]])).to.equal(false);
    });
    it(
      'isRuleMatched should return true checking data ' +
        JSON.stringify(data31) +
        ' against rule ' +
        JSON.stringify(testRuleSet3),
      function () {
        expect(ruleManager.isRuleMatched(data31, testRuleSet3)).to.equal(true);
      }
    );
    it(
      'isRuleMatched should return true checking data ' +
        JSON.stringify(data32) +
        ' against rule ' +
        JSON.stringify(testRuleSet3),
      function () {
        expect(ruleManager.isRuleMatched(data32, testRuleSet3)).to.equal(true);
      }
    );
    it('Should allow to change comparison processor on fly', function () {
      const customComparisonProcessor = {
        isTypeOf: function (
          value: string | number,
          testAgainst: string,
          negation?: boolean
        ): boolean {
          if (negation) {
            return typeof value !== testAgainst;
          }
          return typeof value === testAgainst;
        }
      };
      ruleManager.comparisonProcessor = customComparisonProcessor;
      expect(ruleManager.getComparisonProcessorMethods()).to.have.deep.members(
        Object.getOwnPropertyNames(customComparisonProcessor).filter(
          (name) => typeof customComparisonProcessor[name] === 'function'
        )
      );
    });
    // TODO: Add direct value comparison with no `key` field in rule
  });
});
