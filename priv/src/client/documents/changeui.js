shimi.changeui = (function ()
{
  'use strict';

  var mod = {};
  var index = shimi.index;

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
        if (item.doc.changes !== null)
        {
          item.doc.changes = Object.keys(item.doc.changes).map(function (key)
          {
            return item.doc.changes[key];
          });
        }
      });

      return resp;
    };

    index(
    {
      prefix: 'changelog',
      include_docs: 'true',
      url: url,
      format: format,
      target: target
    }).get(startkey, startid, prevkeys, previds);

    return mod;
  };

  return mod;
})();
