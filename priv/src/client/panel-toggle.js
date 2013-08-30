var panelToggle = (function ()
{
  'use strict';

  var mod = {};

  mod.toggler = function (target)
  {
    var panel;

    if ($(target).attr('data-panel'))
    {
      panel = $('#' + $(target).attr('data-panel'));
    }
    else
    {
      panel = $(target).closest('.panel');
    }

    if (panel.css('display') === 'none')
    {
      panel.css('display', 'table-cell');
    }
    else
    {
      panel.css('display', 'none');
    }

    return mod;
  };

  return mod;
})();

exports(panelToggle);
