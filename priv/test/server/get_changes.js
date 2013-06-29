var should = require('chai').should();
var simple_doc = require('../fixtures/simple_doc').simple_doc;
var simple_doc2 = require('../fixtures/simple_doc2').simple_doc2;
var simple_doc3 = require('../fixtures/simple_doc3').simple_doc3;
var simple_multifieldset_doc = require('../fixtures/simple_multifieldset_doc').simple_multifieldset_doc;
var simple_multifieldset_doc2 = require('../fixtures/simple_multifieldset_doc2').simple_multifieldset_doc2;
var simple_multifieldset_doc3 = require('../fixtures/simple_multifieldset_doc3').simple_multifieldset_doc3;
var get_changes = require('../../src/server/shimi_ima/lib/update_helpers.js').get_changes;

// This is because v8 doesn't have it
Array.concat = function(x, y) {
  'use strict';
  return x.concat(y);
};

describe('CouchDB get_changes function', function() {
  'use strict';
  describe('When making a single change', function() {
    var changes = get_changes(simple_doc2, simple_doc, true);
    it('should record it correctly', function() {
      changes['25250e2ead108a8f60213f2404006a4d'].newValue.should.equal('900');
      changes['25250e2ead108a8f60213f2404006a4d'].originalValue.should.equal('""');
    });
    it('should record it and only it', function() {
      Object.keys(changes).length.should.equal(1);
    });
    it('should contain the fieldset label', function() {
      changes['25250e2ead108a8f60213f2404006a4d'].fieldsetLabel.should.equal('Hip');
    });
    it('should have null for the fieldset instance if it doesn\'t exist', function() {
      should.equal(changes['25250e2ead108a8f60213f2404006a4d'].fieldsetInstance, null);
    });
  });
  describe('When deleting and restoring', function() {
    var changes = get_changes(simple_doc3, simple_doc2, true);
    it('should changes should be null', function() {
      should.equal(changes, null);
    });
  });
  describe('When making multiple changes', function() {
    var changes = get_changes(simple_multifieldset_doc2, simple_multifieldset_doc, true);
    it('should record them correctly', function() {
      changes['6cfbfe0501e6c8b947a4c2cc8941b2da'].newValue.should.equal('"hand"');
      changes['6cfbfe0501e6c8b947a4c2cc8941b2da'].originalValue.should.equal('"plan"');
      changes['6a0b89d82acc684d00feb8b2db5b7f92'].newValue.should.equal('"ballon"');
      changes['6a0b89d82acc684d00feb8b2db5b7f92'].originalValue.should.equal('"nerf"');
    });
  });
  describe('When removing a multifieldset field', function() {
    var changes = get_changes(simple_multifieldset_doc3, simple_multifieldset_doc2, true);
    it('should have an original but not new value', function() {
      should.not.exist(changes['6cfbfe0501e6c8b947a4c2cc8941b2da'].newValue);
      changes['6cfbfe0501e6c8b947a4c2cc8941b2da'].originalValue.should.equal('"hand"');
    });
  });
  describe('When adding a multifieldset field', function() {
    var changes = get_changes(simple_multifieldset_doc2, simple_multifieldset_doc3, true);
    it('should have an new but not original value', function() {
      should.not.exist(changes['6cfbfe0501e6c8b947a4c2cc8941b2da'].originalValue);
      changes['6cfbfe0501e6c8b947a4c2cc8941b2da'].newValue.should.equal('"hand"');
    });
  });
  describe('When creating a document', function() {
    var changes = get_changes(simple_multifieldset_doc2, null, true);
    it('should changes should be null', function() {
      should.equal(changes, null);
    });
  });
});