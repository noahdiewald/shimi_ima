// # New dialog
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Dialog for adding a new user created index.

// ## Variable Definitions

var ihelpers = require('index_tool/ihelpers');
var ilistingui = require('index_tool/ilistingui');
var ajax = require('ajax');
var form = require('form');

// ## Internal Functions

var indexDoctype = function () {
  'use strict';

  return document.getElementById('index-doctype-input');
};

var indexFieldset = function () {
  'use strict';

  return document.getElementById('index-fieldset-input');
};

var indexField = function () {
  'use strict';

  return document.getElementById('index-field-input');
};

var handleChange = function (changed, dependent) {
  'use strict';

  if (changed.value && !changed.value.isBlank()) {
    dependent[0].removeAttribute('disabled');

    Array.prototype.forEach.call(dependent[0].getElementsByTagName('option'), function (item) {
      if (item.classList.contains(changed.value)) {
        form.showEnable(item);
      } else {
        form.hideDisable(item);
      }
    });
  } else {
    dependent.forEach(function (item) {
      item.value = '';
      item.setAttribute('disabled', 'disabled');
    });
  }

  return changed;
};

var getLabelForVal = function (val) {
  'use strict';

  return document.querySelector('#index-new-dialog option[value="' + val + '"]').innerHTML;
};

var getLabel = function (indexFieldset, indexField) {
  'use strict';

  return [getLabelForVal(indexFieldset.value), getLabelForVal(indexField.value)].join(':');
};

// ## Exported Functions

// The dialog for adding a new index.
var initIndexNewDialog = function () {
  'use strict';

  var indexName = document.getElementById('index-name-input');
  var indexShowDeleted = document.getElementById('index-show_deleted-input');

  indexFieldset().setAttribute('disabled', 'disabled');
  indexField().setAttribute('disabled', 'disabled');

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
            'doctype': indexDoctype().value,
            'fields_label': [getLabel(indexFieldset(), indexField())],
            'fields': [indexField().value]
          };
          var complete = function () {
            ilistingui.init();
            dialog.dialog('close');
          };

          ajax.post('indexes', obj, complete);
        }
      },
      'Cancel': function () {
        $(this).dialog('close');
      }
    },
    close: function () {
      var cleared = form.clear(document.querySelectorAll('.input'));
      Array.prototype.forEach.call(cleared, function (item) {
        item.classList.remove('ui-state-error');
      });
    }
  });

  return dialog;
};

var doctypeInputChange = function () {
  'use strict';

  return handleChange(indexDoctype(), [indexFieldset(), indexField()]);
};

var fieldsetInputChange = function () {
  'use strict';

  return handleChange(indexFieldset(), [indexField()]);
};

exports.doctypeInputChange = doctypeInputChange;
exports.fieldsetInputChange = fieldsetInputChange;
exports.initIndexNewDialog = initIndexNewDialog;
