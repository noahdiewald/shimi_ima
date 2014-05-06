// # Replace dialog
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Dialog for providing a function to replace the normal output of
// an index.

// Variable Definitions

var ihelpers = require('index_tool/ihelpers');
var form = require('form');

// Exported functions

// The dialog for providing a function to replace the normal output of
// an index.
var initReplaceDialog = function () {
  'use strict';

  var replaceFunction = $('#index-replace_function-input');
  var indexData = $('#index-editing-data');
  var remove = $('#index-remove_function-input');

  if (indexData.attr('data-index-replace_function')) {
    replaceFunction.val(indexData.attr('data-index-replace_function'));
  } else {
    form.clear(replaceFunction).removeClass('ui-state-error');
  }

  var dialog = $('#index-replace-dialog').dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      'Save': function () {
        $('.input').removeClass('ui-state-error');

        // place holder for client side validation
        var checkResult = true;

        if (!remove.is(':checked')) {
          if (replaceFunction.val().isBlank()) {
            replaceFunction.addClass('ui-state-error');
          } else {
            replaceFunction.removeClass('ui-state-error');
          }

          if (checkResult) {
            indexData.attr('data-index-replace_function', replaceFunction.val());
            $('#replace-function-message').text('This index has a replacement function.');
          }
        } else {
          indexData.removeAttr('data-index-replace_function');
          $('#replace-function-message').empty();
        }

        $(this).dialog('close');
      },
      'Cancel': function () {
        $(this).dialog('close');
      }
    },
    close: function () {
      form.clear(replaceFunction).removeClass('ui-state-error');
    }
  });

  return dialog;
};

exports.initReplaceDialog = initReplaceDialog;
