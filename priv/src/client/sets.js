// # Set operations
//
// The 'set' is a one dimensional Array by default but by replacing the
// `member` function, other types of Arrays may be used.

// Exported functions

// Determine membership of item in the set.
var member = function (arr, x)
{
  var memb = arr.some(function (y)
  {
    return x === y;
  });
  return memb;
};

// Rebuild the array so that all values are unique. This is kind of a
// 'clean up' function used to work around the differences between arrays
// and sets.
var unique = function (x, mem)
{
  if (!mem)
  {
    mem = member;
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

// Return the union of two sets.
var union = function (xs, ys, mem)
{
  if (!mem)
  {
    mem = member;
  }
  var uni = unique(xs.concat(ys), mem);
  return uni;
};

// Return the intersection of two sets.
var intersection = function (xs, ys, mem)
{
  if (!mem)
  {
    mem = member;
  }
  var inter = xs.filter(function (x)
  {
    return mem(ys, x);
  });
  return inter;
};

// Return the relative complement of two sets.
var relativeComplement = function (xs, ys, mem)
{
  if (!mem)
  {
    mem = member;
  }
  var comp = xs.filter(function (x)
  {
    return !mem(ys, x);
  });
  return comp;
};

// Return the symmetric difference of two sets.
var symmetricDifference = function (xs, ys, mem)
{
  if (!mem)
  {
    mem = member;
  }
  var comp1 = relativeComplement(xs, ys, mem);
  var comp2 = relativeComplement(ys, xs, mem);
  var uni = union(comp1, comp2, mem);
  return uni;
};

exports(member);
exports(unique);
exports(union);
exports(intersection);
exports(relativeComplement);
exports(symmetricDifference);
