shimi.dblclickDispatch = function (e)
{
  'use strict';

  var searchui = shimi.searchui;
  var worksheetui = shimi.worksheetui;

  var action = shimi.dispatcher(
  {
    '.search-result-field-id a': function (t)
    {
      searchui.addField($(t).parent('h5'));
    },
    '.field-view b': function (t)
    {
      searchui.addField($(t).parent('li'));
    },
    '.field-container label span': function (t)
    {
      searchui.addField($(t).parent('label').parent('div'));
    },
    '#index-index-input-label': function ()
    {
      searchui.addIndex();
    },
    '.panel > h2': function (t)
    {
      shimi.panelToggle.toggler(t);
    },
    '#toggle-handles': function (t)
    {
      worksheetui.hideHandles();
    },
    '.fieldset-handle': function (t)
    {
      worksheetui.hideFieldset($(t).attr('data-field-fieldset'));
    },
    '.field-handle': function (t)
    {
      worksheetui.hideField($(t).attr('data-field-field'));
    }
  });

  action(e);
};
