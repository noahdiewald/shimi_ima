var should = require('chai').should();
var headed_field = require('../fixtures/headed_field').headed_field;
var stamp = require('../../src/server/shimi_ima/lib/update_helpers.js').stamp;

describe('CouchDB stamp function', function () {
  'use strict';
  describe('when stamping a document of category "field" with property "head" equal to true', function () {
    it('should set the creating user properly', function () {
      var req = {
        userCtx: {
          name: 'bill'
        }
      };
      var testVal = stamp(headed_field, null, req);

      should.exist(testVal);
      should.exist(testVal.doc);
      testVal.doc.created_by_.should.equal('bill');
    });
  });
});
