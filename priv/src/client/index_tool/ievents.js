// # Dialog Events
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// These are change events triggered in the dialogs.

// Variable Definitions

var h = require('./ihelpers.js');

//
// Exported Functions
//

// Set change events for doctype field
var setIndexDoctypeEvents = function (indexDoctype, indexFieldset, callback) {
  'use strict';

  indexDoctype.change(function () {
    var url = 'doctypes/' + indexDoctype.val() + '/fieldsets';
    var callback2;

    if (callback) {
      callback2 = callback();
    }

    h.fOpts(url, indexFieldset, callback2);
  });

  return false;
};

// Set change events for index field
var setIndexFieldsetEvents = function (indexDoctype, indexFieldset, indexField, callback) {
  'use strict';

  indexFieldset.change(function () {
    var callback2;

    if (typeof indexDoctype !== 'string') {
      indexDoctype = indexDoctype.val();
    }

    if (indexFieldset.val()) {
      var url = 'doctypes/' + indexDoctype + '/fieldsets/' + indexFieldset.val() + '/fields?as=options';

      if (callback) {
        callback2 = callback();
      }

      h.fOpts(url, indexField, callback2);
    }
  });

  return true;
};

// Set change events for field
var setIndexFieldEvents = function (indexDoctype, indexFieldset, indexField, callback) {
  'use strict';

  indexField.change(function () {
    var fieldId = indexField.val();
    var fieldsetId = indexFieldset.val();
    var callback2;

    if (callback) {
      callback2 = callback();
    }

    if (!(fieldId.isBlank())) {
      h.getFieldDoc(fieldId, fieldsetId, indexDoctype, function (data) {
        h.alterOpts(data, fieldId, callback2);
      });
    }
  });

  return true;
};

// Set change events for the operator field.
var setIndexOperatorEvents = function (argumentField, operatorField, fieldField, callback) {
  'use strict';

  operatorField.change(function () {
    var callback2;

    if (callback) {
      callback2 = callback();
    }

    h.alterArg(argumentField, operatorField, fieldField, callback2);
  });

  return true;
};

exports.setIndexOperatorEvents = setIndexOperatorEvents;
exports.setIndexFieldEvents = setIndexFieldEvents;
exports.setIndexFieldsetEvents = setIndexFieldsetEvents;
exports.setIndexDoctypeEvents = setIndexDoctypeEvents;
