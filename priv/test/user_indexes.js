var simple_user_index = {
  '_id': '0923ebc77f5e57e0edbe40eed1f282e1',
  '_rev': '9-d20eea26efebb4379e5f20efdd4ae9ad',
  'category': 'index',
  'doctype': 'Entry',
  'show_deleted': false,
  'fields': [
    'd5331cbb4d62fe3d2899f142d90746b7'],
  'fields_label': [
    'Headword:Lexical Category'],
  'name': 'all_vaio',
  'conditions': [{
    'is_or': false,
    'parens': false,
    'negate': false,
    'fieldset': 'd5331cbb4d62fe3d2899f142d9036de5',
    'field': 'd5331cbb4d62fe3d2899f142d90746b7',
    'operator': 'equal',
    'argument': 'vai+o'
  }],
  'expression': '(equals("d5331cbb4d62fe3d2899f142d90746b7","vai + o "))',
  'updated_at_': 'Mon, 10 Dec 2012 01:44:45 GMT',
  'updated_by_': 'admin',
  'created_at_': 'Tue, 20 Nov 2012 15:16:22 GMT',
  'created_by_': 'monica',
  'prev_': '8-e3053a73fcf555a8a8b4e5d035678aaa'
};

Object.prototype.testEnv = true;

var emit = function() {return false};

//= ../src/server/shimi_ima/views/user_indexes/map.js

var assert = require('should');

var mapFunction = function() {
  var fbody = map(simple_user_index).views.index.map;
  return new Function('doc', 'var f = '+ fbody + '; return f(doc);');
};

describe('User index view map funtion', function() {
  describe('when encountering a user index', function() {
    it('should return an object with the correct _id', function() {
      map(simple_user_index)._id.should.equal('_design/' + simple_user_index._id);
    });
    it('should return an object with the correct version', function() {
      map(simple_user_index).version.should.equal(simple_user_index._rev);
    });
    it('should return an object with an evaluable map function', function() {
      (function() {
        mapFunction();
      }).should.not.throwError();
    });
    describe('when evaluating the returned map function of the simple user index', function() {
      it('should return false when the object is not the right doctype', function() {
        mapFunction()({}).should.equal(false);
      });
      it('should return false when the object is deleted', function() {
        var doc = {
          doctype: 'Entry',
          index: [],
          deleted_: true
        };
        mapFunction()({}).should.equal(false);
      });
      it('should return false when the object has no index', function() {
        var doc = {
          doctype: 'Entry',
          deleted_: true
        };
        mapFunction()({}).should.equal(false);
      });
      it('should return \'passed initial if clause\' when the object is the right doctype, is not deleted and has an index', function() {
        var doc = {
          doctype: 'Entry',
          index: [],
          deleted_: false
        };
        mapFunction()({}).should.equal(false);
      });
    });
  });
});
