//= fixtures/bad_format_doc.js
//= fixtures/early_doc.js
//= fixtures/high_integer.js
//= fixtures/late_doc.js
//= fixtures/lower_integer.js
//= fixtures/simple_doc.js
//= fixtures/future_doc.js
//= fixtures/past_doc.js
//= fixtures/today_doc.js
//= toLocaleFormat.js

Object.prototype.testEnv = true;

//= ../src/server/shimi_ima/validate_doc_update.js

var assert = require('should');

var roUser = function () {
  validate(simple_doc, {}, {
    name: 'charlie',
    roles: ['readonly']
  });
};

var testCase = function (newDoc, saveDoc) {
  var testInstance = function () {
    return validate(newDoc, saveDoc, {name: 'charlie', roles: []});
  };
  
  return testInstance;
};

var manipDate = function(days) {
  var myDate = new Date();
  var newDate = myDate.setDate(myDate.getDate() + days);
  
  return new Date(newDate).toLocaleFormat('%Y-%m-%d');
};

describe('CouchDB validation', function () {
  describe('when testing for a valid user', function() {
    it('should block changes by a readonly user', function () {
      roUser.should.throwError(/is a read only user./);      
    });
  });
  describe('when validating documents created by a user', function() {
    describe('in simple cases', function() {
      it('should ignore empty new and saved documents', function () {
        testCase({}, {})().should.equal('skipped');
      });
      it('should ignore the document if the saved document is null', function() {
        testCase({})().should.equal('skipped');
      });
      it('should pass a valid document', function() {
        testCase(simple_doc)().should.equal('ok');
      });
    });
    describe('and dealing with integers', function() {
      it('should reject integers that are above the maximum', function() {
        testCase(high_integer).should.throwError(/Must be less than or equal to 0/);
      });
      it('should reject integers that are below the minimum', function() {
        testCase(lower_integer).should.throwError(/Must be greater than or equal to 0/);
      });
    });
    describe('and dealing with dates', function() {
      it('should reject a bad date format', function() {
        testCase(bad_format_doc).should.throwError(/date must be in format yyyy-mm-dd/);
      });
      it('should reject dates that are below the minimum', function() {
        testCase(early_doc).should.throwError(/date must be later than 1990-09-23./);
      });
      it('should reject dates that are above the maximum', function() {
        testCase(late_doc).should.throwError(/date must be earlier than or equal to 1990-07-23./);
      });
      it('should reject dates that are below the relative minimum, i.e. today', function() {
        testCase(past_doc(manipDate(-1))).should.throwError(/date must be in the future./);
      });
      it('should reject dates that are equal to the relative minimum, i.e. today', function() {
        testCase(past_doc(manipDate(0))).should.throwError(/date must be in the future./);
      });
      it('should reject dates that are above the relative maximum, i.e. today', function() {
        testCase(future_doc(manipDate(1))).should.throwError(/date must be in the past./);
      });
      it('should accept today\'s date when both the maximum and minimum are today', function() {
        testCase(today_doc(manipDate(0)))().should.equal('ok');
      });
      it('should reject tomorrow\'s date when both the maximum and minimum are today', function() {
        testCase(today_doc(manipDate(1))).should.throwError(/date must be today./);
      });
      it('should reject yesterday\'s date when both the maximum and minimum are today', function() {
        testCase(today_doc(manipDate(-1))).should.throwError(/date must be today./);
      });
    });
  });
});