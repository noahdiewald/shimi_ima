shimi.ipreviewui = (function ()
{
  'use strict';

  var mod = {};
  var pager = shimi.pager;

  mod.get = function (startkey, startid, prevkeys, previds)
  {
    var prefix = 'preview';
    var indexId = $('#index-editing-data').attr('data-index-id');
    var url = 'indexes/' + indexId + '/preview';
    var target = $('#' + prefix + '-list-view');

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

    if (indexId)
    {
      pager(
      {
        prefix: prefix,
        format: format,
        url: url,
        origin: 'ipreviewui',
        target: target
      }).get(startkey, startid, prevkeys, previds);
    }

    return mod;
  };

  return mod;
})();
