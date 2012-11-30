shimi.iui = (function() {
  var mod = {};
  var store = shimi.store;
  var flash = shimi.flash;
  var index = shimi.index;

  mod.get = function(startkey, startid, prevkeys, previds) {
    var url = 'documents/index';
    var indexId = $('#index-index-input').val();
    var target = $('#index-listing');

    index({url: url, indexId: indexId, target: target})
      .get(startkey, startid, prevkeys, previds);

    return mod;
  };

  mod.iOpts = function() {
    var url = "/projects/project-" + $('#container').attr('data-project-id') +
        "/indexes?as=options";

    $.get(url, function(data) {
            $('#index-index-input').html(data);
          });

    return mod;
  };
  
  mod.load = function(target) {
    var id = $(target).attr('href').slice(1);
    $("#document-view").html("<em>Loading...</em>");
    shimi.eui.clear();
    shimi.vui({id: id}).get();
  
    return mod;
  };
  
  return mod;
})();
