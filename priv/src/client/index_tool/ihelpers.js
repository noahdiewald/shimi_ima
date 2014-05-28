// # Index tool helpers.
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Shared functions used by a number of index tool modules.

// Variable Definitions

var s = require('sess');
var ajax = require('ajax');
var form = require('form');

// Internal functions

// Disable certain options match `disables`.
var disableOptions = function (options, disables) {
  'use strict';

  Array.prototype.forEach.call(options.childNodes, function (node) {
    node.classList.remove('hidden');
  });

  disables.forEach(function (item) {
    options.querySelector('option:contains(' + item + ')').classList.add('hidden');
  });

  return false;
};

// Disable the operator options.
var disableOperatorOptions = function (fieldDoc) {
  'use strict';

  var options = document.getElementById('builder-operator-input');

  switch (fieldDoc.subcategory) {
  case 'select':
  case 'docselect':
  case 'text':
  case 'textarea':
    disableOptions(options, ['member', 'true']);
    break;
  case 'integer':
  case 'rational':
  case 'date':
    disableOptions(options, ['member', 'true', 'match']);
    break;
  case 'boolean':
  case 'openboolean':
    disableOptions(options, ['equal', 'greater', 'less', 'member', 'match']);
    break;
  case 'multiselect':
  case 'docmultiselect':
    disableOptions(options, ['equal', 'greater', 'less', 'true', 'match']);
    break;
  }

  return false;
};

// Exported functions

// Handles an input field that presents different behavior depending on
// the values of previously filled in fields.
var alterArg = function (argumentField, operatorField, fieldField, callback) {
  'use strict';

  var fieldDoc = function () {
    return s.get(fieldField.value);
  };

  callback();

  // TODO: remove Jquery UI dep.
  try {
    // Destroy these if initialized already
    $(argumentField).removeAttr('disabled').datepicker('destroy');
    $(argumentField).removeAttr('disabled').autocomplete('destroy');
  } catch (err) {
    window.console.log(err.message);
  }

  var dateOrText = function (argumentField, fdoc) {
    if (fdoc.subcategory === 'date') {
      $(argumentField).removeAttr('disabled').datepicker({
        dateFormat: 'yy-mm-dd'
      });
    } else {
      $(argumentField).removeAttr('disabled').autocomplete({
        source: fdoc.allowed
      });
    }

    return true;
  };

  var fdoc = fieldDoc();

  if (fdoc) {
    switch (operatorField.value) {
    case 'true':
    case 'isDefined':
    case 'blank':
      argumentField.setAttribute('disabled', 'disabled').value = '';
      break;
    case 'equal':
    case 'member':
    case 'greater':
    case 'less':
    case 'hasExactly':
    case 'hasGreater':
    case 'hasLess':
      dateOrText(argumentField, fdoc);
      break;
    }

  }

  return true;
};

// Certain operator options only exist for certain types of fields.
var alterOpts = function (fieldDoc, fieldId, callback) {
  'use strict';

  disableOperatorOptions(fieldDoc);
  callback();

  return true;
};

// Get the document holding the field information.
var getFieldDoc = function (fieldId, fieldsetId, doctypeId, callback) {
  'use strict';

  var fieldDoc = s.get(fieldId);
  var url = 'doctypes/' + doctypeId + '/fieldsets/' + fieldsetId + '/fields/' + fieldId + '?format=json';

  if (fieldDoc) {
    if (callback) {
      callback(fieldDoc);
    }
    return fieldDoc;
  } else {
    ajax.get(url, function (req) {
      s.put(req.response);
      if (callback) {
        callback(s.get(fieldId));
      }
    });

    return s.get(fieldId);
  }
};

// Return an object containing methods for working with common events.
var evs = function () {
  'use strict';

  var mod = {};

  mod.setIndexDoctypeEvents = function (indexDoctype, indexFieldset, callback) {
    indexDoctype.change(function () {
      var url = 'doctypes/' + indexDoctype.val() + '/fieldsets';
      var callback2;

      if (callback) {
        callback2 = callback();
      }

      form.fillOptionsFromUrl(url, indexFieldset, callback2);
    });

    return false;
  };

  mod.setIndexFieldsetEvents = function (indexDoctype, indexFieldset, indexField, callback) {
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

        form.fillOptionsFromUrl(url, indexField, callback2);
      }
    };

    return mod;
  };

  mod.setIndexFieldEvents = function (indexDoctype, indexFieldset, indexField, callback) {
    indexField.onchange = function () {
      var fieldId = indexField.value;
      var fieldsetId = indexFieldset.value;
      var callback2;

      if (callback) {
        callback2 = callback();
      }

      if (!(fieldId.isBlank())) {
        getFieldDoc(fieldId, fieldsetId, indexDoctype, function (data) {
          alterOpts(data, fieldId, callback2);
        });
      }
    };

    return mod;
  };

  mod.setIndexOperatorEvents = function (argumentField, operatorField, fieldField, callback) {
    operatorField.onchange = function () {
      var callback2;

      if (callback) {
        callback2 = callback();
      }

      alterArg(argumentField, operatorField, fieldField, callback2);
    };

    return mod;
  };
};

exports.alterArg = alterArg;
exports.alterOpts = alterOpts;
exports.getFieldDoc = getFieldDoc;
exports.evs = evs;
