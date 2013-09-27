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
      testCase(formalize.toForm, null).should.Throw(/invalid JSON: null/);
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
    });
  });
  describe('when provided null', function ()
  {
    it('should return an empty form', function ()
    {
      formalize.toForm('null').should.match(/^\s*<form>\s*<\/form>\s*$/);
    });
  });
  describe('when provided an empty object', function ()
  {
    it('should return a form and an empty list', function ()
    {
      formalize.toForm('{}').should.match(/^\s*<form>\s*<ul>\s*<\/ul>\s*<\/form>\s*$/);
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
    describe('when key value is true', function ()
    {
      it('should return an input type of "text"', function ()
      {
        formalize.toForm('{"test":true}').should.match(/<input [^>]*type="text"/);
      });
      it('should return an input value of true', function ()
      {
        formalize.toForm('{"test":true}').should.match(/<input [^>]*value="true"/);
      });
    });
    describe('when key value is false', function ()
    {
      it('should return an input type of "text"', function ()
      {
        formalize.toForm('{"test":false}').should.match(/<input [^>]*type="text"/);
      });
      it('should return an input value of false', function ()
      {
        formalize.toForm('{"test":false}').should.match(/<input [^>]*value="false"/);
      });
    });
    describe('when key value is null', function ()
    {
      it('should return an input type of "text"', function ()
      {
        formalize.toForm('{"test":null}').should.match(/<input [^>]*type="text"/);
      });
      it('should return an input value of null', function ()
      {
        formalize.toForm('{"test":null}').should.match(/<input [^>]*value="null"/);
      });
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
describe('Converting HTML form to JSON', function ()
{
  'use strict';

  describe('when provided invalid HTML', function ()
  {
    it('should raise an exception', function ()
    {
      testCase(formalize.fromForm, '').should.Throw(/invalid HTML: ""/);
      testCase(formalize.fromForm, undefined).should.Throw(/invalid HTML: non-string/);
      testCase(formalize.fromForm, 'dsfd').should.Throw(/invalid form: no form found/);
    });
  });
  describe('when provided no arguments', function ()
  {
    it('should raise an exception', function ()
    {
      var myTestCase = function ()
      {
        return formalize.fromForm();
      };

      myTestCase.should.Throw(/invalid HTML: non-string/);
    });
  });
  describe('when provided an invalid form', function ()
  {
    describe('with no form tag', function ()
    {
      it('should raise an exception', function ()
      {
        testCase(formalize.fromForm, '<p></p>').should.Throw(/invalid form: no form found/);
      });
    });
    describe('with two form tags', function ()
    {
      it('should raise an exception', function ()
      {
        testCase(formalize.fromForm, '<form><form>').should.Throw(/invalid form: only one form allowed/);
      });
    });
  });
  describe('when provided with an empty form', function ()
  {
    it('should return "null"', function ()
    {
      formalize.fromForm('<form></form>').should.equal('null');
    });
  });
  describe('when provided with an empty unorder list', function ()
  {
    it('should return "{}"', function ()
    {
      formalize.fromForm('<form><ul></ul></form>').should.equal('{}');
    });
  });
  describe('when provided with a form with one field', function ()
  {
    describe('when the field is text input', function ()
    {
      it('should return the proper JSON', function ()
      {
        (typeof formalize.fromForm('<form><ul><input type="text" name="test" value="ok"/></ul></form>')).should.equal('string');
        formalize.fromForm('<form><ul><input type="text" name="test" value="ok"/></ul></form>').should.equal('{"test":"ok"}');
      });
    });
    describe('when the field is number input', function ()
    {
      it('should return the proper JSON', function ()
      {
        (typeof formalize.fromForm('<form><ul><input type="number" name="test" value="7"/></ul></form>')).should.equal('string');
        formalize.fromForm('<form><ul><input type="number" name="test" value="7"/></ul></form>').should.equal('{"test":7}');
      });
    });
    describe('when the field is a textarea', function ()
    {
      it('should return the proper JSON', function ()
      {
        (typeof formalize.fromForm('<form><ul><textarea name="test">ok</textarea></ul></form>')).should.equal('string');
        formalize.fromForm('<form><ul><textarea name="test">ok</textarea></ul></form>').should.equal('{"test":"ok"}');
      });
    });
    describe('when the field value is true', function ()
    {
      it('should return the proper JSON', function ()
      {
        formalize.fromForm('<form><ul><input type="text" name="test" value="true"/></ul></form>').should.equal('{"test":true}');
      });
    });
    describe('when the field value is false', function ()
    {
      it('should return the proper JSON', function ()
      {
        formalize.fromForm('<form><ul><input type="text" name="test" value="false"/></ul></form>').should.equal('{"test":false}');
      });
    });
    describe('when the field value is null', function ()
    {
      it('should return the proper JSON', function ()
      {
        formalize.fromForm('<form><ul><input type="text" name="test" value="null"/></ul></form>').should.equal('{"test":null}');
      });
    });
  });
});
describe('Testing through inversion of toForm', function ()
{
  'use strict';

  var invertTo = function (jsn)
  {
    it('should be equal', function ()
    {
      formalize.fromForm(formalize.toForm(jsn)).should.equal(jsn);
    });
  };
  describe('when the argument is "null"', function ()
  {
    invertTo('null');
  });
  describe('when the argument is "{}"', function ()
  {
    invertTo('{}');
  });
  describe('when it is a single key with a string value', function ()
  {
    invertTo('{"test":"ok"}');
  });
  describe('when it is a single key with a text value', function ()
  {
    invertTo('{"test":"ok00000000000000000000000000000000000000000000000"}');
  });
  describe('when it is a single key with a number value', function ()
  {
    invertTo('{"test":99}');
  });
  describe('when it is a single key with a true value', function ()
  {
    invertTo('{"test":true}');
  });
  describe('when it is a single key with a false value', function ()
  {
    invertTo('{"test":false}');
  });
  describe('when it is a single key with a null value', function ()
  {
    invertTo('{"test":null}');
  });
  describe('when it is multiple keys with values', function ()
  {
    invertTo('{"test":null,"a":1,"b":true,"hippo":"jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj","sevent":"ij"}');
  });
  //describe('when it has a object child', function ()
  //{
  //  invertTo('{"test":null,"m":{"a":1,"b":true},"sevent":"ij"}');
  //});
});
