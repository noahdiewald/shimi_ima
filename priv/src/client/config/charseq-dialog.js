// # Charseq manipulation dialog
//
// *Implicit depends:* DOM, JQuery, JQueryUI

// Variable Definitions

var charseqElems = require('./charseq-elems.js').charseqElems;
var charseqTab = require('./charseq-tab.js');
var form = require('../form.js');

// Exported functions

// Dialog for manipulating doctypes
var charseqDialog = function (values)
{
  'use strict';
  var f = charseqElems.get(values);

  var dialog = $('#charseq-dialog').dialog(
  {
    width: 650,
    autoOpen: false,
    modal: true,
    buttons:
    {
      'Save': function ()
      {
        var obj = f.getCharseqInputVals();
        var url = 'config/charseqs';
        var method = 'POST';
        var complete = function (context)
        {
          charseqTab.init();
          $(context).dialog('close');
        };

        if (values && values.rev)
        {
          method = 'PUT';
          url = 'config/charseqs/' + obj._id + '?rev=' + obj.rev;
        }

        form.send(url, obj, method, complete, this);
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

exports(charseqDialog);
