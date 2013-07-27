shimi.indexui = (function ()
{
  'use strict';

  var mod = {};
  var store = shimi.store;
  var flash = shimi.flash;
  var index = shimi.index;

  mod.get = function (startkey, startid, prevkeys, previds)
  {
    var prefix = 'index';
    var url = 'documents/' + prefix;
    var indexId = $('#index-' + prefix + '-input').val();
    var target = $('#' + prefix + '-listing');

    var format = function (text)
    {
      var resp = JSON.parse(text);

      resp.rows = resp.rows.map(function (item)
      {
        item.display_key = item.key.map(function (k)
        {
          return k[1];
        });

        return item;
      });

      return resp;
    };

    index(
    {
      prefix: prefix,
      format: format,
      url: url,
      origin: 'indexui',
      indexId: indexId,
      target: target
    }).get(startkey, startid, prevkeys, previds);

    return mod;
  };

  mod.iOpts = function ()
  {
    var url = 'indexes?as=options';
    var options;

    $.getJSON(url, function (data)
    {
      options = templates['index-options'].render(data);
      $('#index-index-input').html(options);
    });

    return mod;
  };

  mod.load = function (target)
  {
    var id = $(target).attr('href').slice(1);
    $('#document-view').html('<em>Loading...</em>');
    shimi.editui.clear();
    shimi.viewui.get(id);

    return mod;
  };

  return mod;
})();
