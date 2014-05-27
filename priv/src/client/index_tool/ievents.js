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
var setIndexDoctypeEvents = function (indexDoctype, indexFieldset, callback) {
  'use strict';

  indexDoctype.onchange = function () {
    var url = 'doctypes/' + indexDoctype.value + '/fieldsets';
    var callback2;

    if (callback) {
      callback2 = callback();
    }

    h.fOpts(url, indexFieldset, callback2);
  };

  return false;
};

// Set change events for index field
var setIndexFieldsetEvents = function (indexDoctype, indexFieldset, indexField, callback) {
  'use strict';

  indexFieldset.onchange = function () {
    var callback2;

    if (typeof indexDoctype !== 'string') {
      indexDoctype = indexDoctype.value;
    }

    if (indexFieldset.value) {
      var url = 'doctypes/' + indexDoctype + '/fieldsets/' + indexFieldset.value + '/fields?as=options';

      if (callback) {
        callback2 = callback();
      }

      h.fOpts(url, indexField, callback2);
    }
  };

  return true;
};

// Set change events for field
var setIndexFieldEvents = function (indexDoctype, indexFieldset, indexField, callback) {
  'use strict';

  indexField.onchange = function () {
    var fieldId = indexField.value;
    var fieldsetId = indexFieldset.value;
    var callback2;

    if (callback) {
      callback2 = callback();
    }

    if (!(fieldId.isBlank())) {
      h.getFieldDoc(fieldId, fieldsetId, indexDoctype, function (data) {
        h.alterOpts(data, fieldId, callback2);
      });
    }
  };

  return true;
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
