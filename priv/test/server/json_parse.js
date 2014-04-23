var should = require('chai').should();
var json_parse = require('../../src/server/shimi_ima/lib/json_parse.js');

var testCase = function (fun, arg) {
  'use strict';

  return function () {
    fun(arg);
  };
};

describe('Converting JSON to an AST', function () {
  'use strict';

  describe('when provided invalid JSON', function () {
    it('should raise an exception', function () {
      testCase(json_parse.tryParseJSON, '').should.Throw(/invalid JSON: ""/);
      testCase(json_parse.tryParseJSON, undefined).should.Throw(/invalid JSON: undefined/);
      testCase(json_parse.tryParseJSON, {}).should.Throw(/invalid JSON: {}/);
      testCase(json_parse.tryParseJSON, null).should.Throw(/invalid JSON: null/);
    });
  });
  describe('when provided no arguments', function () {
    it('should raise an exception', function () {
      var myTestCase = function () {
        return json_parse.tryParseJSON();
      };

      myTestCase.should.Throw(/invalid JSON: undefined/);
    });
  });
  describe('when provided invalid type of argument', function () {
    it('should raise an exception', function () {
      testCase(json_parse.validate, 'string').should.Throw(/cannot build AST from: string/);
      testCase(json_parse.validate, []).should.Throw(/cannot build AST from: array/);
      testCase(json_parse.validate, 1).should.Throw(/cannot build AST from: number/);
    });
  });
});
