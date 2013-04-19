//= fixtures/simple_doc.js
//= fixtures/simple_doc2.js
//= fixtures/simple_doc3.js
//= fixtures/simple_multifieldset_doc.js
//= fixtures/simple_multifieldset_doc2.js
//= fixtures/simple_multifieldset_doc3.js

// Avoid having to clutter the global name space and get lint complaints
Object.prototype.testEnv = true;

// This is because v8 doesn't have it
Array.concat = function(x, y) {
  return x.concat(y);
};

//= ../src/server/shimi_ima/lib/fields.js
//= ../src/server/shimi_ima/lib/update_helpers.js

Object.prototype.foldFields = fromFieldsetsFold;

var should = require("should");

describe("CouchDB get_changes function", function() {
  describe("When making a single change", function() {
    var changes = get_changes(simple_doc2, simple_doc);
    it("should record it correctly", function() {
      changes["25250e2ead108a8f60213f2404006a4d"].newValue.should.equal("900");
      changes["25250e2ead108a8f60213f2404006a4d"].originalValue.should.equal("\"\"");
    });
    it("should record it and only it", function() {
      Object.keys(changes).length.should.equal(1);
    });
    it("should contain the fieldset label", function() {
      changes["25250e2ead108a8f60213f2404006a4d"].fieldsetLabel.should.equal("Hip");
    });
    it("should have null for the fieldset instance if it doesn't exist", function() {
      should.strictEqual(changes["25250e2ead108a8f60213f2404006a4d"].fieldsetInstance, null);
    });
  });
  describe("When deleting and restoring", function() {
    var changes = get_changes(simple_doc3, simple_doc2);
    it("should contain 00000000000000000000000000000000", function() {
      should.exist(changes["00000000000000000000000000000000"]);
    });
    it("should report deletion", function() {
      changes["00000000000000000000000000000000"].deleted.should.be.true;
    });
  });
  describe("When making multiple changes", function() {
    var changes = get_changes(simple_multifieldset_doc2, simple_multifieldset_doc);
    it("should record them correctly", function() {
      changes["6cfbfe0501e6c8b947a4c2cc8941b2da"].newValue.should.equal("\"hand\"");
      changes["6cfbfe0501e6c8b947a4c2cc8941b2da"].originalValue.should.equal("\"plan\"");
      changes["6a0b89d82acc684d00feb8b2db5b7f92"].newValue.should.equal("\"ballon\"");
      changes["6a0b89d82acc684d00feb8b2db5b7f92"].originalValue.should.equal("\"nerf\"");
    });
  });
  describe("When removing a multifieldset field", function() {
    var changes = get_changes(simple_multifieldset_doc3, simple_multifieldset_doc2);
    it("should have an original but not new value", function() {
      should.not.exist(changes["6cfbfe0501e6c8b947a4c2cc8941b2da"].newValue);
      changes["6cfbfe0501e6c8b947a4c2cc8941b2da"].originalValue.should.equal("\"hand\"");
    });
  });
  describe("When adding a multifieldset field", function() {
    var changes = get_changes(simple_multifieldset_doc2, simple_multifieldset_doc3);
    it("should have an new but not original value", function() {
      should.not.exist(changes["6cfbfe0501e6c8b947a4c2cc8941b2da"].originalValue);
      changes["6cfbfe0501e6c8b947a4c2cc8941b2da"].newValue.should.equal("\"hand\"");
    });
  });
  describe("When creating a document", function() {
    var changes = get_changes(simple_multifieldset_doc2, null);
    it("should report creation", function() {
      changes["00000000000000000000000000000000"].created.should.be.true;
    });
  });
});
