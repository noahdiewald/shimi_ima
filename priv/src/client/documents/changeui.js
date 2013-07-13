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

    index(
    {
      prefix: 'changelog',
      url: url,
      target: target
    }).get(startkey, startid, prevkeys, previds);

    return mod;
  };

  return mod;
})();