import 'mocha';
import {expect} from 'chai';

import {parsePreviewParam} from '../src/parse-preview-param';

interface PreviewParamCase {
  name: string;
  input: unknown;
  expected: {experienceId: string; variationId: string} | null;
}

const CASES: PreviewParamCase[] = [
  {
    name: 'parses a well-formed numeric experienceId.variationId pair',
    input: '123.456',
    expected: {experienceId: '123', variationId: '456'}
  },
  {
    name: 'returns null for an empty string',
    input: '',
    expected: null
  },
  {
    name: 'returns null when there is no dot separator',
    input: '123',
    expected: null
  },
  {
    name: 'returns null when the variationId segment is empty',
    input: '123.',
    expected: null
  },
  {
    name: 'returns null when the experienceId segment is empty',
    input: '.456',
    expected: null
  },
  {
    name: 'returns null when both segments are non-numeric',
    input: 'a.b',
    expected: null
  },
  {
    name: 'returns null when a segment is partially non-numeric',
    input: '12a.34',
    expected: null
  },
  {
    name: 'returns null when there is more than one dot',
    input: '1.2.3',
    expected: null
  },
  {
    name: 'returns null for a whitespace-only string',
    input: '  ',
    expected: null
  },
  {
    name: 'returns null for null input',
    input: null,
    expected: null
  },
  {
    name: 'returns null for undefined input',
    input: undefined,
    expected: null
  },
  {
    name: 'returns null for a non-string input',
    input: 123456,
    expected: null
  }
];

describe('parsePreviewParam', function () {
  CASES.forEach(({name, input, expected}) => {
    it(name, function () {
      const result = parsePreviewParam(input as unknown as string);
      if (expected === null) {
        expect(result).to.be.null;
      } else {
        expect(result).to.deep.equal(expected);
      }
    });
  });
});
