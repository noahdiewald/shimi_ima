shimi.iiui = (function () {
  var mod = {};

  mod.init = function () {
    var url = "indexes";
    var target = $('#index-index-listing');

    $.get(url, function (index) {
      target.html(index);
    });

    return mod;
  };

  return mod;
})();