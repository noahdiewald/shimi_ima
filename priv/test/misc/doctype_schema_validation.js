var tv4 = require('tv4').tv4;
var should = require('chai').should();
var simple_doc = require('../fixtures/simple_doc').simple_doc;
var simple_schema = require('../fixtures/simple_schema').simple_schema;

describe('Doctype schemas', function ()
{
  'use strict';
  describe('when testing a simple schema against a simple doc', function ()
  {
    it('should validate the simple doc', function ()
    {
      tv4.validate(simple_doc, simple_schema.schema).should.equal(true);
    });

    it('should be invalid with a bad created_at_ date', function ()
    {
      var tmp_simple = JSON.parse(JSON.stringify(simple_doc));
      tmp_simple.created_at_ = 'bad date';

      tv4.validate(tmp_simple, simple_schema.schema);
      tv4.error.should.equal(true);
    });
  });
});
