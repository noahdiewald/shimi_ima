shimi.changeui = (function ()
{
  'use strict';

  var mod = {};
  var pager = shimi.pager;

  mod.get = function (startkey, startid, prevkeys, previds)
  {
    var prefix = 'changelog';
    var url = prefix;
    var target = $('#' + prefix + '-listing');

    var format = function (text)
    {
      var resp = JSON.parse(text);

      resp.rows.map(function (item)
      {
        if (item.doc.changes)
        {
          item.doc.changes = Object.keys(item.doc.changes).map(function (key)
          {
            return item.doc.changes[key];
          });
        }
      });

      return resp;
    };

    pager(
    {
      prefix: 'changelog',
      origin: 'changeui',
      url: url,
      format: format,
      target: target
    }).get(startkey, startid, prevkeys, previds);

    return mod;
  };

  return mod;
})();
