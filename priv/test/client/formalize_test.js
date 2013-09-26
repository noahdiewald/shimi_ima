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
      formalize.toForm('{"test": "ok"}').should.match(/^\s*<form(.|\n)*<\/form>\s*$/);
    });
    it('should return a label named for the key', function ()
    {
      formalize.toForm('{"test": "ok"}').should.match(/>test<\/label>/);
      formalize.toForm('{"test": "ok"}').should.match(/<label for="test">/);
    });
    it('should return an input named for the key', function ()
    {
      formalize.toForm('{"test": "ok"}').should.match(/<input [^>]*name="test"/);
    });
    it('should return an unordered list with a single element', function ()
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
    describe('when key value is a string of greater than 32 characters', function ()
    {
      it('should return a textarea', function ()
      {
        formalize.toForm('{"test": "0123456789012345678901234567890123"}').should.match(/<textarea name="test">[0-9]{34}<\/textarea>/);
      });
    });
    describe('when key value is a number', function ()
    {
      it('should return an input type of "number"', function ()
      {
        formalize.toForm('{"test": 23}').should.match(/<input [^>]*type="number"/);
      });
    });
  });
  describe('when provided an object with multiple keys', function ()
  {
    it('should return a label named for each key', function ()
    {
      formalize.toForm('{"a": 1, "b": 2, "c": "3"}').should.match(/>a<\/label>/);
      formalize.toForm('{"a": 1, "b": 2, "c": "3"}').should.match(/<label for="a">/);
      formalize.toForm('{"a": 1, "b": 2, "c": "3"}').should.match(/>b<\/label>/);
      formalize.toForm('{"a": 1, "b": 2, "c": "3"}').should.match(/<label for="b">/);
    });
    it('should return an input named for each key', function ()
    {
      formalize.toForm('{"a": 1, "b": 2, "c": "3"}').should.match(/<input [^>]*name="a"/);
      formalize.toForm('{"a": 1, "b": 2, "c": "3"}').should.match(/<input [^>]*name="b"/);
      formalize.toForm('{"a": 1, "b": 2, "c": "3"}').should.match(/<input [^>]*name="c"/);
    });
    it('should return an unordered list with the correct number of elements', function ()
    {
      formalize.toForm('{"a": 1, "b": 2, "c": "3"}').match(/<ul>/g).length.should.equal(1);
      formalize.toForm('{"a": 1, "b": 2, "c": "3"}').match(/<li>/g).length.should.equal(3);
    });
  });
});
