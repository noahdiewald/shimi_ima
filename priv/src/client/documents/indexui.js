shimi.indexiu = (function () {
  var mod = {};
  var store = shimi.store;
  var flash = shimi.flash;
  var index = shimi.index;

  mod.get = function (startkey, startid, prevkeys, previds) {
    var url = 'documents/index';
    var indexId = $('#index-index-input').val();
    var target = $('#index-listing');

    index({
      url: url,
      indexId: indexId,
      target: target
    }).get(startkey, startid, prevkeys, previds);

    return mod;
  };

  mod.iOpts = function () {
    var url = "indexes?as=options";
    var options;

    $.getJSON(url, function (data) {
      options = templates['index-options'].render(data);
      $('#index-index-input').html(options);
    });

    return mod;
  };

  mod.load = function (target) {
    var id = $(target).attr('href').slice(1);
    $("#document-view").html("<em>Loading...</em>");
    shimi.editui.clear();
    shimi.viewui.get(id);

    return mod;
  };

  return mod;
})();