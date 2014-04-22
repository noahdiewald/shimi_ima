var should = require('chai').should();
var list = require('../../src/server/shimi_ima/lib/index_csv.js').list;

var start = function (obj) {
  'use strict';

  return obj;
};

var makeSend = function (acc) {
  'use strict';

  return function (obj) {
    acc.push(obj);
  };
};

var makeGetRow = function (items) {
  'use strict';

  items.reverse();

  return function () {
    return items.pop();
  };
};

describe('Listing database items as CSVs', function () {
  'use strict';

  describe('when there is no item', function () {
    it('should send a string', function () {
      var acc = [];
      list(null, null, start, makeSend(acc), makeGetRow([]));
      (typeof acc[0]).should.equal('string');
    });
    it('should have column labels', function () {
      var acc = [];
      list(null, null, start, makeSend(acc), makeGetRow([]));
      acc[0].should.equal('ID,KEY,VALUE\n');
    });
  });
  describe('when there is only a single item', function () {
    it('should send two strings', function () {
      var acc = [];
      var data = JSON.parse('[{"id": "r", "key": "b","value": "c"}]');
      list(null, null, start, makeSend(acc), makeGetRow(data));
      (typeof acc[0]).should.equal('string');
      (typeof acc[1]).should.equal('string');
    });
    it('should send the correct values', function () {
      var acc = [];
      var data = JSON.parse('[{"id": "r", "key": "b","value": "c"}]');
      list(null, null, start, makeSend(acc), makeGetRow(data));
      acc[0].should.equal('ID,KEY,VALUE\n');
      acc[1].should.equal('"r","b","c"\n');
    });
  });
});
