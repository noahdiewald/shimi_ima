// # Dialog Events
//
// *Implicit depends:* DOM
//
// These are change events triggered in the dialogs.

// ## Variable Definitions

var h = require('index_tool/ihelpers');

// ## Exported Functions

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
