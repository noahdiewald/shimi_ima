// # Doctype manipulation dialog
//
// *Implicit depends:* DOM, JQuery, JQueryUI

// Variable Definitions

var doctypeElems = require('./doctype-elems.js').doctypeElems;
var doctypeTab = require('./doctype-tab.js');

// Exported functions

// Dialog for manipulating doctypes
var doctypeDialog = function (url, values)
{
  'use strict';

  var f = doctypeElems.get(values);

  if (values.rev && !values.rev.isBlank())
  {
    f.doctype.attr('disabled', 'disabled');
  }
  else
  {
    f.doctype.removeAttr('disabled');
  }

  var dialog = $('#doctype-dialog').dialog(
  {
    autoOpen: false,
    modal: true,
    buttons:
    {
      'Save': function ()
      {
        var obj = f.getDoctypeInputVals();
        var complete = function (context)
        {
          doctypeTab.init();
          $(context).dialog('close');
        };

        if (!values.rev || values.rev.isBlank())
        {
          url.post(obj, complete, this);
        }
        else
        {
          obj._id = url.doctype;
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

exports.doctypeDialog = doctypeDialog;
