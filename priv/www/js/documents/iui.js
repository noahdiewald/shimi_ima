ʃimi.iui = function() {
  var mod = {};
  mod.url = 'documents/index';
  mod.indexId = $('#index-index-input').val();
  mod.target = $('#index-listing');

  mod.get = function(startkey, startid, prevkeys, previds) {
    index({url: mod.url, indexId: mod.indexId, target: mod.target})
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
};
