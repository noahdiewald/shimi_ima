var console = require('console');
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
  describe('when recursing a simple object', function ()
  {
    it('should return a transformed object', function ()
    {
      var origObj1 = JSON.stringify({
        cat: 'sat',
        mat: {coat: 'cap'}
      });
      var transObj1 = JSON.stringify({
        CAT: 'SAT',
        MAT: {COAT: 'CAP'}
      });
      var transObj2 = JSON.stringify({
        fields: [
          {key: 'cat', string: true, number: false, array: false, object: false, value: 'sat'},
          {key: 'mat', string: false, number: false, array: false, object: true, value: [
            {key: 'coat', string: true, number: false, array: false, object: false, value: 'cap'}
          ]}
        ]
      });

      var getKeyVals = function (o)
      {
        return Object.keys(o).map(function (k)
        {
          var val = o[k];

          return {
            key: k,
            string: (typeof val === 'string'),
            number: (typeof val === 'number'),
            array: (val instanceof Array),
            object: ((val instanceof Object) && !(val instanceof Array)),
            value: val
          };
        });
      };

      var transform1 = function (o)
      {
        var transform_ = function (o, rest, accObj, id)
        {
          var result;
          var keyVals = getKeyVals(o);

          result = keyVals.reduce(function (acc, x)
          {
            var newKey = x.key.toUpperCase();
            var oldKey = x.key;
            var value = x.value;

            o[newKey] = value;
            delete o[oldKey];

            if (x.string)
            {
              o[newKey] = value.toUpperCase();
              return acc;
            }
            else
            {
              return acc.concat(value);
            }
          }, []);

          rest = rest.concat(result);

          if (rest.length !== 0)
          {
            return transform_.r(result[0], rest.slice(1), accObj, id);
          }
          else
          {
            return id.r(accObj);
          }
        };

        return transform_.t(o, [], o, recurse.identity);
      };

      var transform2 = function (o)
      {
        var start = {fields: []};

        var transform_ = function (o, rest, accObj, id)
        {
          var result;
          var keyVals = getKeyVals(o.object);

          result = keyVals.reduce(function (acc, x, i)
          {
            if (x.object)
            {
              return acc.concat({object: x.value, key: 'value', parent: x});
            }
            else
            {
              return acc;
            }
          }, []);

          rest = rest.concat(result);
          o.parent[o.key] = keyVals;

          if (rest.length !== 0)
          {
            return transform_.r(rest[0], rest.slice(1), accObj, id);
          }
          else
          {
            return id.r(accObj);
          }
        };

        return transform_.t({object: o, parent: start, key: 'fields'}, [], start, recurse.identity);
      };

      JSON.stringify(transform1(JSON.parse(origObj1))).should.be.equal(transObj1);
      JSON.stringify(transform2(JSON.parse(origObj1))).should.be.equal(transObj2);
    });
  });
  describe('when recursing a complex object', function ()
  {
    it('should return a transformed object', function ()
    {
      var origObj1 = JSON.stringify({
        cat: 'sat',
        mat: {coat: 'cap'},
        lap: {hide: [1,2,3]},
        snif: [4,5,{b: 'right', back: ['o','k','?']}]
      });
      var transObj1 = JSON.stringify({
        fields: [
          {key: 'cat', index: false, string: true, number: false, array: false, object: false, value: 'sat'},
          {key: 'mat', index: false, string: false, number: false, array: false, object: true, value: [
            {key: 'coat', index: false, string: true, number: false, array: false, object: false, value: 'cap'}
          ]},
          {key: 'lap', index: false, string: false, number: false, array: false, object: true, value: [
            {key: 'hide', index: false, string: false, number: false, array: true, object: false, value: [
              {key: false, index: 0, string: false, number: true, array: false, object: false, value: 1},
              {key: false, index: 1, string: false, number: true, array: false, object: false, value: 2},
              {key: false, index: 2, string: false, number: true, array: false, object: false, value: 3}
            ]}
          ]},
          {key: 'snif', index: false, string: false, number: false, array: true, object: false, value: [
            {key: false, index: 0, string: false, number: true, array: false, object: false, value: 4},
            {key: false, index: 1, string: false, number: true, array: false, object: false, value: 5},
            {key: false, index: 2, string: false, number: false, array: false, object: true, value: [
              {key: 'b', index: false, string: true, number: false, array: false, object: false, value: 'right'},
              {key: 'back', index: false, string: false, number: false, array: true, object: false, value: [
                {key: false, index: 0, string: true, number: false, array: false, object: false, value: 'o'},
                {key: false, index: 1, string: true, number: false, array: false, object: false, value: 'k'},
                {key: false, index: 2, string: true, number: false, array: false, object: false, value: '?'}
              ]}
            ]}
          ]}
        ]
      });

      var getKeyVals = function (o)
      {
        return Object.keys(o).map(function (k)
        {
          var val = o[k];

          return {
            key: (o instanceof Array) ? false : k,
            index: (o instanceof Array) ? k * 1 : false,
            string: (typeof val === 'string'),
            number: (typeof val === 'number'),
            array: (val instanceof Array),
            object: ((val instanceof Object) && !(val instanceof Array)),
            value: val
          };
        });
      };

      var transform1 = function (o)
      {
        var start = {fields: []};

        var transform_ = function (o, rest, accObj, id)
        {
          var result;
          var keyVals = getKeyVals(o.object);

          result = keyVals.reduce(function (acc, x)
          {
            if (x.object || x.array)
            {
              return acc.concat({object: x.value, key: 'value', parent: x});
            }
            else
            {
              return acc;
            }
          }, []);

          rest = rest.concat(result);
          o.parent[o.key] = keyVals;

          if (rest.length !== 0)
          {
            return transform_.r(rest[0], rest.slice(1), accObj, id);
          }
          else
          {
            return id.r(accObj);
          }
        };

        return transform_.t({object: o, parent: start, key: 'fields'}, [], start, recurse.identity);
      };

      JSON.stringify(transform1(JSON.parse(origObj1))).should.be.equal(transObj1);
    });
  });
});