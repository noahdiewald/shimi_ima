// TODO: This is not a complete set of test but there is enough
// overlap with the formalize.js tests on the client side that I think
// the coverage is sufficient.

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
      testCase(json_parse.parse, '').should.Throw(/invalid JSON: ""/);
      testCase(json_parse.parse, undefined).should.Throw(/invalid JSON: undefined/);
      testCase(json_parse.parse, {}).should.Throw(/invalid JSON: {}/);
      testCase(json_parse.parse, null).should.Throw(/invalid JSON: null/);
    });
  });
  describe('when provided no arguments', function () {
    it('should raise an exception', function () {
      var myTestCase = function () {
        return json_parse.parse();
      };

      myTestCase.should.Throw(/invalid JSON: undefined/);
    });
  });
  describe('when provided JSON null argument', function () {
    it('should return the empty object', function () {
      JSON.stringify(json_parse.parse('null')).should.equal('{}');
    });
  });
  describe('when provided invalid type of argument', function () {
    it('should raise an exception', function () {
      testCase(json_parse.parse, '"string"').should.Throw(/cannot build AST from: string/);
      testCase(json_parse.parse, '[]').should.Throw(/cannot build AST from: array/);
      testCase(json_parse.parse, '1').should.Throw(/cannot build AST from: number/);
    });
  });
  describe('when provided simple objects with a single string value', function () {
    it('should have a top level key "fields"', function () {
      JSON.stringify(json_parse.parse('{"a":"b"}')).should.match(/^{"fields":\[/);
    });
    it('should return an AST with a correctly labeled type', function () {
      JSON.stringify(json_parse.parse('{"a":"b"}')).should.match(/"type":"string"/);
    });
    it('should have a false index', function () {
      JSON.stringify(json_parse.parse('{"a":"b"}')).should.match(/"index":false/);
    });
    it('should have the correct key', function () {
      JSON.stringify(json_parse.parse('{"a":"b"}')).should.match(/"key":"a"/);
    });
    it('should have the correct value', function () {
      JSON.stringify(json_parse.parse('{"a":"b"}')).should.match(/"value":"b"/);
    });
  });
  describe('when provided simple objects with a single number value', function () {
    it('should return an AST with a correctly labeled type', function () {
      JSON.stringify(json_parse.parse('{"a":1}')).should.match(/"type":"number"/);
    });
    it('should have the correct value', function () {
      JSON.stringify(json_parse.parse('{"a":1}')).should.match(/"value":1/);
    });
  });
  describe('when provided simple objects with a single null value', function () {
    it('should return an AST with a correctly labeled type', function () {
      JSON.stringify(json_parse.parse('{"a":null}')).should.match(/"type":"null"/);
    });
    it('should have the correct value', function () {
      JSON.stringify(json_parse.parse('{"a":null}')).should.match(/"value":null/);
    });
  });
  describe('when provided simple objects with an empty object value', function () {
    it('should return an AST with a correctly labeled type', function () {
      JSON.stringify(json_parse.parse('{"a":{}}')).should.match(/"type":"object"/);
    });
    it('should have the correct value', function () {
      JSON.stringify(json_parse.parse('{"a":{}}')).should.match(/"value":\[\]/);
    });
  });
  describe('when provided simple objects with an empty array value', function () {
    it('should return an AST with a correctly labeled type', function () {
      JSON.stringify(json_parse.parse('{"a":[]}')).should.match(/"type":"array"/);
    });
    it('should have the correct value', function () {
      JSON.stringify(json_parse.parse('{"a":[]}')).should.match(/"value":\[\]/);
    });
  });
  describe('when provided an object with an array value', function () {
    it('should have false keys and proper index values', function () {
      JSON.stringify(json_parse.parse('{"a":[1,2,3]}')).should.equal('{"fields":[{"key":"a","index":false,"type":"array","value":[{"key":false,"index":0,"type":"number","value":1},{"key":false,"index":1,"type":"number","value":2},{"key":false,"index":2,"type":"number","value":3}]}]}');
    });
  });
});
