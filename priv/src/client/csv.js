shimi.sets = (function () {
  var mod = {};

  mod.arraysToCSV = function (a) {
    return a.map(function (x) {
      return x.map(function (y) {
        return '"' + y.toString().replace(/"/, '""') + '"';
      }).join(",");
    }).join("\n");
  };

  return mod;
})();