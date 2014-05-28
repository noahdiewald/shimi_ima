// # Dialog Events
//
// *Implicit depends:* DOM
//
// These are change events triggered in the dialogs.

// Variable Definitions

var h = require('index_tool/ihelpers');

//
// Exported Functions
//

// Set change events for doctype field
var setChangeEvent = function (changed, dependent) {
  'use strict';

  indexDoctype.onchange = function () {
    if (changed.value && !changed.value.isBlank()) {
      dependent.removeAttribute('disabled');

      Array.prototype.forEach.call(dependent.getElementsByTagName('option'), function (item) {
        if (item.classList.contains(changed.value)) {
          item.classList.remove('hidden');
          item.removeAttribute('disabled');
        } else {
          item.classList.add('hidden');
          item.setAttribute('disabled', 'disabled');
        }
      });
    } else {
      dependent.value = '';
      dependent.setAttribute('disabled', 'disabled');
    }
  };

  return false;
};

// Set change events for the operator field.
var setIndexOperatorEvents = function (argumentField, operatorField, fieldField, callback) {
  'use strict';

  operatorField.onchange = function () {
    var callback2;

    if (callback) {
      callback2 = callback();
    }

    h.alterArg(argumentField, operatorField, fieldField, callback2);
  };

  return true;
};

exports.setIndexOperatorEvents = setIndexOperatorEvents;
exports.setIndexFieldEvents = setIndexFieldEvents;
exports.setIndexFieldsetEvents = setIndexFieldsetEvents;
exports.setIndexDoctypeEvents = setIndexDoctypeEvents;
