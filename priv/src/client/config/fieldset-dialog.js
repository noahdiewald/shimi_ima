// # Fieldset manipulation dialog
//
// *Implicit depends:* DOM, JQuery, JQueryUI

// Variable Definitions

var fieldsetElems = require('./fieldset-elems.js').fieldsetElems;
var doctypeTab = require('./doctype-tab.js');

// Exported functions

// Dialog for manipulating fieldsets
var fieldsetDialog = function (url, values) {
  'use strict';

  var f = fieldsetElems.get(values);

  var dialog = $('#fieldset-dialog').dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      'Save': function () {
        var obj = f.getFieldsetInputVals();
        var complete = function (context) {
          url.fieldset = false;
          url.rev = false;

          doctypeTab.initFieldsets(url);
          $(context).dialog('close');
        };
        if (!values.rev || values.rev.isBlank()) {
          url.post(obj, complete, this);
        } else {
          obj._id = url.fieldset;
          url.put(obj, complete, this);
        }
      },
      'Cancel': function () {
        $(this).dialog('close');
      }
    },
    close: function () {
      f.clear();
    }
  });

  return dialog;
};

exports.fieldsetDialog = fieldsetDialog;
