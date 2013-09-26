var should = require('chai').should();
var recurse = require('../../src/client/recurse.js');

describe('Tail recursion', function ()
{
  'use strict';

  describe('when recursing a nested array', function ()
  {
    it('should return the array items in the correct order', function ()
    {
      var nestedArray = [1,2,[3,[4,5],6],7,[8,[9,[10,11,[12,13]]]],14,15,16];
      var unnested = '1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16';
      var walk = function (a)
      {
        var walk1 = function(h, t, acc, id)
        {
          if (h === undefined && t.length === 0)
          {
            return id.r(acc);
          }
          else if (typeof h === 'number')
          {
            return walk1.r(t[0], t.slice(1), acc.concat(h), id);
          }

          return walk1.r(h[0], h.slice(1).concat(t), acc, id);
        };

        return walk1.t(a[0], a.slice(1), [], recurse.identity);
      };

      walk(nestedArray).join(',').should.equal(unnested);
    });
  });
  describe('when computing factorial', function ()
  {
    it('should complete', function ()
    {
      var factorial = function (a)
      {
        var factorial1 = function (rem, acc, id)
        {
          if (rem === 0)
          {
            return id.r(acc);
          }

          return factorial1.r(rem - 1, rem * acc, id);
        };

        return factorial1.t(a, 1, recurse.identity);
      };

      factorial(20).should.equal(2432902008176640000);
    });
  });
});