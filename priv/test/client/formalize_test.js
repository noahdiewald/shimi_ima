var tv4 = require('tv4').tv4;
var should = require('chai').should();
var formalize = require('../../src/client/formalize.js');

var testCase = function (fun, arg)
{
  'use strict';

  return function ()
  {
    fun(arg);
  };
};

describe('Converting JSON to an HTML form', function ()
{
  'use strict';

  describe('when provided invalid JSON', function ()
  {
    it('should raise an exception', function ()
    {
      testCase(formalize.toForm, '').should.Throw(/invalid JSON: ""/);
      testCase(formalize.toForm, undefined).should.Throw(/invalid JSON: undefined/);
      testCase(formalize.toForm, {}).should.Throw(/invalid JSON: {}/);
    });
  });
  describe('when provided no arguments', function ()
  {
    it('should raise an exception', function ()
    {
      var myTestCase = function ()
      {
        return formalize.toForm();
      };

      myTestCase.should.Throw(/invalid JSON: undefined/);
    });
  });
  describe('when provided invalid type of argument', function ()
  {
    it('should raise an exception', function ()
    {
      testCase(formalize.toForm, '"string"').should.Throw(/cannot build form from: string/);
      testCase(formalize.toForm, '[]').should.Throw(/cannot build form from: array/);
      testCase(formalize.toForm, '1').should.Throw(/cannot build form from: number/);
      testCase(formalize.toForm, 'null').should.Throw(/cannot build form from: null/);
      testCase(formalize.toForm, '{}').should.Throw(/cannot build form from: empty object/);
    });
  });
  describe('when provided an object with a single key', function ()
  {
    it('should return a string', function ()
    {
      (typeof formalize.toForm('{"test": "ok"}')).should.be.equal('string');
    });
    it('should return a form', function ()
    {
      formalize.toForm('{"test": "ok"}').should.match(/^\s*<form.*<\/form>\s*$/);
    });
    it('should return have a label named for the key', function ()
    {
      formalize.toForm('{"test": "ok"}').should.match(/>test<\/label>/);
      formalize.toForm('{"test": "ok"}').should.match(/<label for="test">/);
    });
    it('should return have an input named for the key', function ()
    {
      formalize.toForm('{"test": "ok"}').should.match(/<input [^>]*name="test"/);
    });
    it('should return have an unordered list with a single element', function ()
    {
      formalize.toForm('{"test": "ok"}').match(/<ul>/g).length.should.equal(1);
      formalize.toForm('{"test": "ok"}').match(/<li>/g).length.should.equal(1);
    });
    describe('when key value is a string of less than 32 characters', function ()
    {
      it('should return an input type of "text"', function ()
      {
        formalize.toForm('{"test": "ok"}').should.match(/<input [^>]*type="text"/);
      });
      it('should return an input with the correct value', function ()
      {
        formalize.toForm('{"test": "ok"}').should.match(/<input [^>]*value="ok"/);
      });
    });
  });
});
