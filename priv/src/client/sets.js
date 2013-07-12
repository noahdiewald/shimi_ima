shimi.sets = (function ()
{
  'use strict';

  var mod = {};

  mod.member = function (arr, x)
  {
    var memb = arr.some(function (y)
    {
      return x === y;
    });
    return memb;
  };

  mod.unique = function (x, mem)
  {
    if (!mem)
    {
      mem = mod.member;
    }
    var uniq = x.reduce(function (acc, curr)
    {
      if (mem(acc, curr))
      {
        return acc;
      }
      else
      {
        return acc.concat([curr]);
      }
    }, []);
    return uniq;
  };

  mod.union = function (xs, ys, mem)
  {
    if (!mem)
    {
      mem = mod.member;
    }
    var uni = mod.unique(xs.concat(ys), mem);
    return uni;
  };

  mod.intersection = function (xs, ys, mem)
  {
    if (!mem)
    {
      mem = mod.member;
    }
    var inter = xs.filter(function (x)
    {
      return mem(ys, x);
    });
    return inter;
  };

  mod.relativeComplement = function (xs, ys, mem)
  {
    if (!mem)
    {
      mem = mod.member;
    }
    var comp = xs.filter(function (x)
    {
      return !mem(ys, x);
    });
    return comp;
  };

  mod.symmetricDifference = function (xs, ys, mem)
  {
    if (!mem)
    {
      mem = mod.member;
    }
    var comp1 = mod.relativeComplement(xs, ys, mem);
    var comp2 = mod.relativeComplement(ys, xs, mem);
    var uni = mod.union(comp1, comp2, mem);
    return uni;
  };

  return mod;
})();
