// # Dispatching double click events
//
// *Implicit depends:* DOM, JQuery, JQueryUI
//
// Almost all double click events that are handled by the system are
// listed here. With [sender.js](./sender.html) and other dispatchers,
// the hope is to centralize effects and interdependencies. At some
// point I may use a more sophisticated approach.

// ## Variable Definitions

var dispatcher = require('./dispatcher.js').dispatcher;
var panelToggler = require('./panel-toggle.js').panelToggler;
var searchui = require('./documents/searchui.js');
var worksheetui = require('./documents/worksheetui.js');

// ## Exported Functions

// Given a click event, determine what action to take based on the
// click target.
var dblclickDispatch = function (e)
{
  'use strict';

  var action = dispatcher(
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
      panelToggler(t);
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

exports.dblclickDispatch = dblclickDispatch;
