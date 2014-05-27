// # New dialog
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Dialog for adding a new user created index.

// TODO I would rather avoid having this as a JQuery plugin.

require('jquery-ui-input-state');

// Variable Definitions

var ihelpers = require('index_tool/ihelpers');
var ilistingui = require('index_tool/ilistingui');
var form = require('form');
var evs = require('index_tool/ievents');

// Exported functions

// The dialog for adding a new index.
var initIndexNewDialog = function () {
  'use strict';

  var indexDoctype = document.getElementByID('index-doctype-input');
  var indexFieldset = document.getElementByID('index-fieldset-input');
  var indexField = document.getElementByID('index-field-input');
  var indexName = document.getElementByID('#index-name-input');
  var indexShowDeleted = $('#index-show_deleted-input');

  indexFieldset.setAttribute('disabled', 'disabled');
  indexField.setAttribute('disabled', 'disabled');

  var doctypeEvents = function () {
    evs.setIndexDoctypeEvents(indexDoctype, indexFieldset, function () {
      indexFieldset.setAttribute('disabled', 'disabled');
      indexField.setAttribute('disabled', 'disabled');

      return function () {
        indexFieldset.removeAttribute('disabled');
      };
    });
  };

  var fieldsetEvents = function () {
    evs.setIndexFieldsetEvents(indexDoctype, indexFieldset, indexField, function () {
      indexField.setAttribute('disabled', 'disabled');

      return function () {
        indexField.removeAttribute('disabled');
      };
    });
  };

  var getLabelForVal = function (val) {
    return document.querySelector('#index-new-dialog option[value="' + val + '"]').innerHTML;
  };

  var getLabel = function () {
    return [getLabelForVal(indexFieldset.value), getLabelForVal(indexField.value)].join(':');
  };

  var dialog = $('#index-new-dialog').dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      'Create': function () {
        Array.prototype.forEach.call(document.querySelectorAll('.input'), function (item) {
          item.classList.remove('ui-state-error');
        });

        // place holder for client side validation
        var checkResult = true;

        if (checkResult) {
          var obj = {
            'category': 'index',
            'name': indexName.value,
            'show_deleted': indexShowDeleted.checked,
            'conditions': [],
            'doctype': indexDoctype.value,
            'fields_label': [getLabel()],
            'fields': [indexField.value]
          },
            complete = function (context) {
              ilistingui.init();
              $(context).dialog('close');
            };
          form.send('indexes', obj, 'POST', complete, this);
        }
      },
      'Cancel': function () {
        $(this).dialog('close');
      }
    },
    close: function () {
      indexFieldset.onchange = undefined;
      indexDoctype.onchange = undefined;
      var cleared = form.clear(document.querySelectorAll('.input'));
      Array.prototype.forEach.call(cleared, function (item) {
        item.classList.remove('ui-state-error');
      });
    }
  });

  doctypeEvents();
  fieldsetEvents();

  return dialog;
};

exports.initIndexNewDialog = initIndexNewDialog;
