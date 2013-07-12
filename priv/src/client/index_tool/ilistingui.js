shimi.ilistingui = (function ()
{
  'use strict';

  var mod = {};

  mod.init = function ()
  {
    var url = 'indexes';
    var target = $('#index-index-listing');
    var listing;

    $.getJSON(url, function (data)
    {
      listing = templates['index-listing'].render(data);
      target.html(listing);
    });

    return mod;
  };

  return mod;
})();
