var simple_doc = {
  "_id": "25250e2ead108a8f60213f2404007c91",
  "_rev": "1-6ec90301ac115fed382a77207e81a9ca",
  "doctype": "bim",
  "description": "",
  "fieldsets": [{
    "id": "b9ad37ea17a58d9be32160f393770e5d",
    "multiple": false,
    "collapse": false,
    "name": "hip",
    "label": "Hip",
    "order": 50,
    "fields": [{
      "id": "25250e2ead108a8f60213f24040007e4",
      "name": "caltest",
      "label": "CalTest",
      "head": false,
      "reversal": false,
      "required": false,
      "min": "",
      "max": "",
      "instance": "25250e2ead108a8f60213f2404005d38",
      "regex": "",
      "order": 50,
      "subcategory": "date",
      "value": "1990-08-23"
    },
    {
      "id": "25250e2ead108a8f60213f240400248f",
      "name": "ff",
      "label": "FF",
      "head": false,
      "reversal": false,
      "required": false,
      "min": 0,
      "max": 10,
      "instance": "25250e2ead108a8f60213f2404006a4d",
      "regex": "",
      "order": 50,
      "subcategory": "integer",
      "value": ""
    },
    {
      "id": "b9ad37ea17a58d9be32160f393771cdd",
      "name": "yer",
      "label": "Yer",
      "head": true,
      "reversal": false,
      "required": false,
      "min": "",
      "max": "",
      "instance": "25250e2ead108a8f60213f240400717c",
      "regex": "",
      "order": 50,
      "subcategory": "boolean",
      "value": false
    }]
  }],
  "created_at_": "Tue Aug 23 2011 22:03:24 GMT-0500 (CDT)",
  "created_by_": "admin"
};
var simple_multifieldset_doc = {
  "_id": "68b6ef8471724e551c7428f8650367df",
  "_rev": "2-3013dd31a27379111586d03d06329bd2",
  "description": "I'm real sour on all this.",
  "doctype": "Sour",
  "created_at_": "Wed, 06 Feb 2013 02:47:16 GMT",
  "created_by_": "admin",
  "updated_at_": "Fri, 19 Apr 2013 03:30:21 GMT",
  "updated_by_": "admin",
  "prev_": "1-efc065e8c0c85059dffc40b11aee111f",
  "deleted_": false,
  "fieldsets": [{
    "id": "ddbcb7e9814be078df660ee6e9032e57",
    "multiple": true,
    "collapse": false,
    "name": "climp",
    "label": "Climp",
    "order": 50,
    "multifields": [{
      "fields": [{
        "id": "1a22914ae12176903f953c060294464f",
        "name": "seven",
        "label": "Seven",
        "head": true,
        "reversal": false,
        "required": false,
        "min": "",
        "max": "",
        "instance": "6cfbfe0501e6c8b947a4c2cc8941b2da",
        "charseq": null,
        "regex": "",
        "order": 50,
        "subcategory": "text",
        "value": "plan",
        "sortkey": ""
      }]
    }, {
      "fields": [{
        "id": "1a22914ae12176903f953c060294464f",
        "name": "seven",
        "label": "Seven",
        "head": true,
        "reversal": false,
        "required": false,
        "min": "",
        "max": "",
        "instance": "7d41ecb5b802930928313f9c95706296",
        "charseq": null,
        "regex": "",
        "order": 50,
        "subcategory": "text",
        "value": "fan",
        "sortkey": ""
      }]
    }]
  }, {
    "id": "dfbc77b0972b3a9ed03602deb8f71122",
    "multiple": false,
    "collapse": false,
    "name": "dimple",
    "label": "Dimple",
    "order": 50,
    "fields": [{
      "id": "ce2a7aa6cdf5ba6607761195f8ab5726",
      "name": "nix",
      "label": "Nix",
      "head": false,
      "reversal": false,
      "required": false,
      "min": "",
      "max": "",
      "instance": "6a0b89d82acc684d00feb8b2db5b7f92",
      "charseq": null,
      "regex": "",
      "order": 50,
      "subcategory": "text",
      "value": "nerf",
      "sortkey": ""
    }]
  }],
  "index": {
    "1a22914ae12176903f953c060294464f": [
      [
        "",
        "plan"],
      [
        "",
        "fan"]
    ],
    "ce2a7aa6cdf5ba6607761195f8ab5726": [
      "",
      "nerf"]
  },
  "changes": null,
  "head": [
    "1a22914ae12176903f953c060294464f"],
  "reverse": []
};

Object.prototype.testEnv = true;
// This is because v8 doesn't have it
Array.concat = function(x, y) {
  return x.concat(y);
};

var identity = function(value) {
  return value;
};

var fromFieldset = function(fieldset, mapfun) {
  var map = function(fields) {
    return fields.map(function(x) {
      return mapfun(x, fieldset);
    });
  };
  
  if (fieldset.multiple) {
    return fieldset.multifields.reduce(function(acc, multifield) {
      return Array.concat(acc, map(multifield.fields));
    }, []);
  } else {
    return map(fieldset.fields);
  }
};

var fromFieldsetsMapFold = function(fieldsets, mapfun, foldfun, init) {
  if (init === undefined) {
    init = [];
  }
  return fieldsets.reduce(function(acc, fieldset) {
    return foldfun(acc, fromFieldset(fieldset, mapfun), fieldset);
  }, init);
};

var fromFieldsets = function(fieldsets) {
  return fromFieldsetsMapFold(fieldsets, identity, function(acc, fields) {
    return Array.concat(acc, fields);
  });
};

var fromFieldsetsFold = function(fieldsets, foldfun, init) {
  return fromFieldsetsMapFold(fieldsets, identity, foldfun, init);
};

var fromFieldsetsMap = function(fieldsets, mapfun) {
  return fromFieldsetsMapFold(fieldsets, mapfun, function(acc, fields) {
    return Array.concat(acc, fields);
  });
};

exports.fromFieldsetsMapFold = fromFieldsetsMapFold;
exports.fromFieldsets = fromFieldsets;
exports.fromFieldsetsFold = fromFieldsetsFold;
exports.fromFieldsetsMap = fromFieldsetsMap;

var assert = require("should");

describe("CouchDB shared field functions", function() {
  describe("when extracting fields from a fieldset", function() {
    it("should have the correct number of fields", function() {
      fromFieldsets(simple_doc.fieldsets).length.should.equal(3);
      fromFieldsets(simple_multifieldset_doc.fieldsets).length.should.equal(3);
    });
  });
  describe("when performing a map on fields to their values", function() {
    var getVal = function(x) {
      return x.value;
    };
    it("should return a list of values with the correct length", function() {
      fromFieldsetsMap(simple_doc.fieldsets, getVal).length.should.equal(3);
      fromFieldsetsMap(simple_multifieldset_doc.fieldsets, getVal).length.should.equal(3);
    });
    it("should return a list of correct values in the order they appear in the document", function() {
      var simp = fromFieldsetsMap(simple_doc.fieldsets, getVal)
      simp[0].should.equal("1990-08-23");
      simp[1].should.equal("");
      simp[2].should.equal(false);
      var mult = fromFieldsetsMap(simple_multifieldset_doc.fieldsets, getVal);
      mult[0].should.equal("plan");
      mult[1].should.equal("fan");
      mult[2].should.equal("nerf");
    });
  });
  describe("when performing a fold on fields to create an object", function() {
    var addTo = function(acc, fields) {
      fields.forEach(function(x) {
        acc[x.instance] = x.value;
      });
      return acc;
    };
    it("should create the expected object", function() {
      var simp = fromFieldsetsFold(simple_doc.fieldsets, addTo, {});
      simp["25250e2ead108a8f60213f2404005d38"].should.equal("1990-08-23");
      simp["25250e2ead108a8f60213f2404006a4d"].should.equal("");
      simp["25250e2ead108a8f60213f240400717c"].should.equal(false);
      var mult = fromFieldsetsFold(simple_multifieldset_doc.fieldsets, addTo, {});
      mult["6cfbfe0501e6c8b947a4c2cc8941b2da"].should.equal("plan");
      mult["7d41ecb5b802930928313f9c95706296"].should.equal("fan");
      mult["6a0b89d82acc684d00feb8b2db5b7f92"].should.equal("nerf");
    });
  });
});