// # Field manipulation dialog
//
// *Implicit depends:* DOM, JQuery, JQueryUI

// Variable Definitions

var fieldElems = require('./field-elems.js');
var doctypeTab = require('./doctype-tab.js');

// Exported functions

// Dialog for manipulating fields
var fieldDialog = function (url, values)
{
  'use strict';

  var f = fieldElems.get(values);

  var dialog = $('#field-dialog').dialog(
  {
    autoOpen: false,
    modal: true,
    buttons:
    {
      'Save': function ()
      {
        var obj = f.clearDisabled().getFieldInputVals();
        var complete = function (context)
        {
          doctypeTab.initFields(url);
          $(context).dialog('close');
        };
        if (!values.rev || values.rev.isBlank())
        {
          url.post(obj, complete, this);
        }
        else
        {
          obj._id = url.field;
          url.put(obj, complete, this);
        }
      },
      'Cancel': function ()
      {
        $(this).dialog('close');
      }
    },
    close: function ()
    {
      f.clear();
    }
  });

  return dialog;
};

exports(fieldDialog);
