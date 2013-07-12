var should = require('chai').should();
var simple_user_index = require('../fixtures/simple_user_index').simple_user_index;
var map = require('../../src/server/shimi_ima/views/lib/user_indexes.js').user_indexes;

var emit = function ()
{
  'use strict';
  return false;
};

var mapFunction = function ()
{
  'use strict';
  var fbody = map(simple_user_index, emit, true).views.index.map;
  /*jshint -W054 */
  return new Function('doc', 'var f = ' + fbody + '; return f(doc, true);');
};

describe('User index view map funtion', function ()
{
  'use strict';
  describe('when encountering a user index', function ()
  {
    it('should return an object with the correct _id', function ()
    {
      map(simple_user_index, emit, true)._id.should.equal('_design/' + simple_user_index._id);
    });
    it('should return an object with the correct version', function ()
    {
      map(simple_user_index, emit, true).version.should.equal(simple_user_index._rev);
    });
    it('should return an object with an evaluable map function', function ()
    {
      var f = function ()
      {
        mapFunction();
      };
      f.should.not.Throw();
    });
    describe('when evaluating the returned map function of the simple user index', function ()
    {
      it('should return false when the object is not the right doctype', function ()
      {
        mapFunction()(
        {}).should.equal(false);
      });
      it('should return false when the object is deleted', function ()
      {
        var doc = {
          doctype: 'Entry',
          index: [],
          deleted_: true
        };
        mapFunction()(
        {}).should.equal(false);
      });
      it('should return false when the object has no index', function ()
      {
        var doc = {
          doctype: 'Entry',
          deleted_: true
        };
        mapFunction()(
        {}).should.equal(false);
      });
      it('should return \'passed initial if clause\' when the object is the right doctype, is not deleted and has an index', function ()
      {
        var doc = {
          doctype: 'Entry',
          index: [],
          deleted_: false
        };
        mapFunction()(
        {}).should.equal(false);
      });
    });
  });
});
