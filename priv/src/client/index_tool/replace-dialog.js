// # Replace dialog
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Dialog for providing a function to replace the normal output of
// an index.

// Variable Definitions

var ihelpers = require('index_tool/ihelpers');

// Exported functions

// The dialog for providing a function to replace the normal output of
// an index.
var initReplaceDialog = function () {
  'use strict';

  var replaceFunction = document.getElementById('index-replace_function-input');
  var indexData = document.getElementById('index-editing-data');
  var remove = document.getElementById('index-remove_function-input');
  var dialogElem = document.getElementById('index-replace-dialog');
  var message = document.getElementById('replace-function-message');

  if (indexData.dataset.indexReplace_function) {
    replaceFunction.value = indexData.dataset.indexReplace_function;
  } else {
    replaceFunction = '';
    replaceFunction.classList.remove('ui-state-error');
  }

  var dialog = $(dialogElem).dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      'Save': function () {

        Array.prototype.forEach.call(document.getElementsByClassName('input'), function (item) {
          item.classList.remove('ui-state-error');
        });

        // place holder for client side validation
        var checkResult = true;

        if (!remove.checked) {
          if (replaceFunction.value.isBlank()) {
            replaceFunction.classList.add('ui-state-error');
          } else {
            replaceFunction.classList.remove('ui-state-error');
          }

          if (checkResult) {
            indexData.dataset.indexReplace_function = replaceFunction.value;
            message.innerHTML = 'This index has a replacement function.';
          }
        } else {
          indexData.removeAttr('data-index-replace_function');
          message.innerHTML = '';
        }

        $(this).dialog('close');
      },
      'Cancel': function () {
        $(this).dialog('close');
      }
    },
    close: function () {
      replaceFunction.value = '';
      replaceFunction.classList.remove('ui-state-error');
    }
  });

  return dialog;
};

exports.initReplaceDialog = initReplaceDialog;
