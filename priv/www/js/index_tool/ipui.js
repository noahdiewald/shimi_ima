shimi.piui = function() {
  var mod = {};
  var index = shimi.index;

  mod.get = function(startkey, startid, prevkeys, previds) {
    var url = 'documents/index';
    var indexId = $('#index-editing-data').attr('data-index-id');
    var target = $('#index-list-view');
    var filterForm = $('#index-filter-form input');
    
    index({url: mod.url, indexId: indexId, target: target})
      .get(startkey, startid, prevkeys, previds);

    filterForm.keyup(function() {mod.get();});

    return mod;
  };
  
  return mod;
};
