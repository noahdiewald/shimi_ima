shimi.piui = (function () {
  var mod = {};
  var index = shimi.index;

  mod.get = function (startkey, startid, prevkeys, previds) {
    var indexId = $('#index-editing-data').attr('data-index-id');
    var url = 'indexes/' + indexId + "/view";
    var target = $('#index-list-view');
    var filterForm = $('#index-filter-form input');

    if (indexId) {
      index({
        url: url,
        target: target
      }).get(startkey, startid, prevkeys, previds);
    }

    return mod;
  };

  return mod;
})();