var should = require('chai').should();
var bad_format_doc = require('../fixtures/bad_format_doc').bad_format_doc;
var early_doc = require('../fixtures/early_doc').early_doc;
var high_integer = require('../fixtures/high_integer').high_integer;
var late_doc = require('../fixtures/late_doc').late_doc;
var lower_integer = require('../fixtures/lower_integer').lower_integer;
var simple_doc = require('../fixtures/simple_doc').simple_doc;
var future_doc = require('../fixtures/future_doc').future_doc;
var past_doc = require('../fixtures/past_doc').past_doc;
var today_doc = require('../fixtures/today_doc').today_doc;
var validate_doc_update = require('../../src/server/shimi_ima/lib/validate_doc_update.js').validate_doc_update;

require('../toLocaleFormat').toLocaleFormat();

var roUser = function ()
{
  'use strict';
  return validate_doc_update(simple_doc,
  {},
  {
    name: 'charlie',
    roles: ['readonly']
  }, true);
};

var testCase = function (newDoc, saveDoc)
{
  'use strict';
  var testInstance = function ()
  {
    return validate_doc_update(newDoc, saveDoc,
    {
      name: 'charlie',
      roles: []
    }, true);
  };

  return testInstance;
};

var manipDate = function (days)
{
  'use strict';
  var myDate = new Date();
  var newDate = myDate.setDate(myDate.getDate() + days);

  return new Date(newDate).toLocaleFormat('%Y-%m-%d');
};

describe('CouchDB validation', function ()
{
  'use strict';
  describe('when testing for a valid user', function ()
  {
    it('should block changes by a readonly user', function ()
    {
      roUser.should.Throw(/is a read only user./);
    });
  });
  describe('when validating documents created by a user', function ()
  {
    describe('in simple cases', function ()
    {
      it('should ignore empty new and saved documents', function ()
      {
        testCase(
        {},
        {})().should.equal('skipped');
      });
      it('should ignore the document if the saved document is null', function ()
      {
        testCase(
        {})().should.equal('skipped');
      });
      it('should pass a valid document', function ()
      {
        testCase(simple_doc)().should.equal('ok');
      });
    });
    describe('and dealing with integers', function ()
    {
      it('should reject integers that are above the maximum', function ()
      {
        testCase(high_integer).should.Throw(/Must be less than or equal to 0/);
      });
      it('should reject integers that are below the minimum', function ()
      {
        testCase(lower_integer).should.Throw(/Must be greater than or equal to 0/);
      });
    });
    describe('and dealing with dates', function ()
    {
      it('should reject a bad date format', function ()
      {
        testCase(bad_format_doc).should.Throw(/date must be in format yyyy-mm-dd/);
      });
      it('should reject dates that are below the minimum', function ()
      {
        testCase(early_doc).should.Throw(/date must be later than 1990-09-23./);
      });
      it('should reject dates that are above the maximum', function ()
      {
        testCase(late_doc).should.Throw(/date must be earlier than or equal to 1990-07-23./);
      });
      it('should reject dates that are below the relative minimum, i.e. today', function ()
      {
        testCase(past_doc(manipDate(-1))).should.Throw(/date must be in the future./);
      });
      it('should reject dates that are equal to the relative minimum, i.e. today', function ()
      {
        testCase(past_doc(manipDate(0))).should.Throw(/date must be in the future./);
      });
      it('should reject dates that are above the relative maximum, i.e. today', function ()
      {
        testCase(future_doc(manipDate(1))).should.Throw(/date must be in the past./);
      });
      it('should accept today\'s date when both the maximum and minimum are today', function ()
      {
        testCase(today_doc(manipDate(0)))().should.equal('ok');
      });
      it('should reject tomorrow\'s date when both the maximum and minimum are today', function ()
      {
        testCase(today_doc(manipDate(1))).should.Throw(/date must be today./);
      });
      it('should reject yesterday\'s date when both the maximum and minimum are today', function ()
      {
        testCase(today_doc(manipDate(-1))).should.Throw(/date must be today./);
      });
    });
  });
});
