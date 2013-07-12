var should = require('chai').should();
var simple_doc = require('../fixtures/simple_doc').simple_doc;
var simple_multifieldset_doc = require('../fixtures/simple_multifieldset_doc').simple_multifieldset_doc;
var fromFieldsets = require('../../src/server/shimi_ima/lib/fields.js').fromFieldsets;
var fromFieldsetsMap = require('../../src/server/shimi_ima/lib/fields.js').fromFieldsetsMap;
var fromFieldsetsFold = require('../../src/server/shimi_ima/lib/fields.js').fromFieldsetsFold;

// This is because v8 doesn't have it
Array.concat = function (x, y)
{
  'use strict';
  return x.concat(y);
};

describe('CouchDB shared field functions', function ()
{
  'use strict';
  describe('when extracting fields from a fieldset', function ()
  {
    it('should have the correct number of fields', function ()
    {
      fromFieldsets(simple_doc.fieldsets).length.should.equal(3);
      fromFieldsets(simple_multifieldset_doc.fieldsets).length.should.equal(3);
    });
  });
  describe('when performing a map on fields to their values', function ()
  {
    var getVal = function (x)
    {
      return x.value;
    };
    it('should return a list of values with the correct length', function ()
    {
      fromFieldsetsMap(simple_doc.fieldsets, getVal).length.should.equal(3);
      fromFieldsetsMap(simple_multifieldset_doc.fieldsets, getVal).length.should.equal(3);
    });
    it('should return a list of correct values in the order they appear in the document', function ()
    {
      var simp = fromFieldsetsMap(simple_doc.fieldsets, getVal);
      simp[0].should.equal('1990-08-23');
      simp[1].should.equal('');
      simp[2].should.equal(false);
      var mult = fromFieldsetsMap(simple_multifieldset_doc.fieldsets, getVal);
      mult[0].should.equal('plan');
      mult[1].should.equal('fan');
      mult[2].should.equal('nerf');
    });
  });
  describe('when performing a fold on fields to create an object', function ()
  {
    var addTo = function (acc, fields)
    {
      fields.forEach(function (x)
      {
        acc[x.instance] = x.value;
      });
      return acc;
    };
    it('should create the expected object', function ()
    {
      var simp = fromFieldsetsFold(simple_doc.fieldsets, addTo,
      {});
      simp['25250e2ead108a8f60213f2404005d38'].should.equal('1990-08-23');
      simp['25250e2ead108a8f60213f2404006a4d'].should.equal('');
      simp['25250e2ead108a8f60213f240400717c'].should.equal(false);
      var mult = fromFieldsetsFold(simple_multifieldset_doc.fieldsets, addTo,
      {});
      mult['6cfbfe0501e6c8b947a4c2cc8941b2da'].should.equal('plan');
      mult['7d41ecb5b802930928313f9c95706296'].should.equal('fan');
      mult['6a0b89d82acc684d00feb8b2db5b7f92'].should.equal('nerf');
    });
  });
});
