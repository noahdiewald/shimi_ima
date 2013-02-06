shimi.ilistingui = (function () {
  var mod = {};

  mod.init = function () {
    var url = "indexes";
    var target = $('#index-index-listing');
    var listing;

    $.getJSON(url, function (data) {
      listing = JST['priv/templates/index-listing'](data);
      target.html(listing);
    });

    return mod;
  };

  return mod;
})();