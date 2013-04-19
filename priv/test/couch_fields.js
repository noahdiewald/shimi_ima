//= fixtures/simple_doc.js
//= fixtures/simple_multifieldset_doc.js

Object.prototype.testEnv = true;
// This is because v8 doesn't have it
Array.concat = function(x, y) {
  return x.concat(y);
};

//= ../src/server/shimi_ima/lib/fields.js

var assert = require("should");

describe("CouchDB shared field functions", function() {
  describe("when extracting fields to an array", function() {
    it("should have the correct number of fields", function() {
      fromFieldsets(simple_doc.fieldsets).length.should.equal(3);
      fromFieldsets(simple_multifieldset_doc.fieldsets).length.should.equal(3);
    });
  });
});