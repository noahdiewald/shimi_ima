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

  describe('when there is only a single item', function () {
    it('should return a string', function () {
      var acc = [];
      list(null, null, start, makeSend(acc), makeGetRow([{
        id: 'r',
        key: 'b',
        value: 'c'
      }]));
      (typeof acc[0]).should.equal('string');
    });
  });
});
