shimi.sets = (function () {
  var mod = {};

  var sessionKey = function () {
    return shimi.documents.identifier() + "_sets";
  };

  var member = function (arr, x) {
    return arr.some(function (y) {
      return x[0] === y[0] && x[1] === y[1];
    });
  };

  var unique = function (x) {
    return x.reduce(function (acc, curr) {
      if (member(acc, curr)) {
        return acc;
      } else {
        return acc.concat([curr]);
      }
    }, []);
  };

  var union = function (xs, ys) {
    return unique(xs.concat(ys));
  };

  var intersection = function (xs, ys) {
    return xs.filter(function (x) {
      return member(ys, x);
    });
  };

  var relativeComplement = function (xs, ys) {
    return xs.filter(function (x) {
      return !member(ys, x);
    });
  };

  var processSet = function (set) {
    var name = set[0];
    var arr = unique(set[1]);
    var procSet = [name, arr];
    return procSet;
  };

  mod.union = function (setNameA, setNameB) {
    var setElemsA = mod.getSet(setNameA)[1];
    var setElemsB = mod.getSet(setNameB)[1];
    var newSet = union(setElemsA, setElemsB);
    return newSet;
  };

  mod.intersection = function (setNameA, setNameB) {
    var setElemsA = mod.getSet(setNameA)[1];
    var setElemsB = mod.getSet(setNameB)[1];
    var newSet = intersection(setElemsA, setElemsB);
    return newSet;
  };

  // To be basically read as A minus B.
  mod.relativeComplement = function (setNameA, setNameB) {
    var setElemsA = mod.getSet(setNameA)[1];
    var setElemsB = mod.getSet(setNameB)[1];
    var newSet = relativeComplement(setElemsA, setElemsB);
    return newSet;
  };

  mod.symetricDifference = function (setNameA, setNameB) {
    var setElemsA = mod.getSet(setNameA)[1];
    var setElemsB = mod.getSet(setNameB)[1];
    var newSet = union(relativeComplement(setElemsA, setElemsB), relativeComplement(setElemsB, setElemsA));
    return newSet;
  };

  mod.getSets = function () {
    var curr = window.sessionStorage.getItem(sessionKey());
    var retval = [];

    if (curr !== null) {
      retval = JSON.parse(curr);
    }

    return retval;
  };

  mod.getSet = function (setName) {
    var retval;
    var curr = mod.getSets();
    retval = curr.filter(function (x) {
      return x[0] === setName;
    })[0];
    return retval;
  };

  mod.removeSet = function (setName) {
    var nnew;
    var curr = mod.getSets();
    nnew = curr.filter(function (x) {
      return x[0] !== setName;
    });
    mod.setSets(nnew);
    return mod;
  };

  mod.getSetNames = function () {
    var curr = mod.getSets();
    return curr.map(function (x) {
      return x[0];
    });
  };

  mod.setSets = function (nnew) {
    var procSets;
    if (Array.isArray(nnew)) {
      procSets = nnew.map(function (x) {
        return processSet(x);
      });
      window.sessionStorage.setItem(sessionKey(), JSON.stringify(procSets));
    } else {
      window.sessionStorage.settem(sessionKey(), "[]");
    }

    return mod;
  };

  mod.setSet = function (nnew) {
    if (Array.isArray(nnew) && nnew.length === 2) {
      var curr = mod.getSets();
      var newName = nnew[0];
      var filtered = curr.filter(function (x) {
        return x[0] !== newName;
      });
      mod.setSets(filtered.concat([nnew]));
    }
    return mod;
  };

  return mod;
})();