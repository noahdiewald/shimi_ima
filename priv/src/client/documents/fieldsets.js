// # Fieldsets (and fields)
//
// *Implicit depends:* DOM
//
// Dealing with fields and fieldsets.

// Variable Definitions

var path = require('../path.js').path;
var store = require('store').store;
var utils = require('utils');
var editui = require('./editui.js');
var info = require('documents/information');
var ajax = require('ajax');
var templates = require('templates');
var dateOrNumber;
var getEncoded;
var getFieldValue;
var fillFields;
var setFieldValue;
var initFieldset;

// Internal functions

// Get the container for a fieldset with `id`.
var fsContainer = function (id) {
  'use strict';

  return document.getElementById('container-' + id);
};

// Get the doctype path.
var dpath = function (source, category) {
  'use strict';

  var url = path(source, category);
  url.doctype = false;
  return url;
};

// If the item referred to by `key` is in session storage perform the
// `success` action with the stored items as the argument, otherwise,
// get the item from the document info and render it.
// WARNING: This exists as part of a minimal change to support a very
// different way of managing this information. It will be rewritten
// soon.
var ifStoredElse = function (key, success, otherwise) {
  'use strict';

  var item;
  var id = key.replace(/^fieldsets\/([^/]*)(\/fields)*$/, '$1');
  var fieldset;

  item = sessionStorage.getItem(key);

  if (item) {
    success(item);
  } else {
    fieldset = info.info().fieldsets.filter(function (x) {
      return x._id === id;
    })[0];

    otherwise(fieldset);
  }
};

// Get the allowed file values from the server.
var getFileAllowed = function (field, callback) {
  'use strict';

  // This is unimplemented.
  return function () {
    callback();
  };
};

// Get the allowed doc values from the server.
var getAllowed = function (field, callback) {
  'use strict';

  var url = '/projects/project-' + info.project() + '/doctypes/' + field.source + '/documents/index';

  return function () {
    ajax.get(url, function (req) {
      var rows = req.response.rows;

      field.allowed = rows.length > 0 ? rows.map(function (x) {
        var value = x.key.map(function (y) {
          return y[1];
        }).join(', ');

        return {
          value: value,
          is_default: value === field['default']
        };
      }) : null;

      return callback();
    });
  };
};

// Process the listing of allowed values.
var processAllowed = function (field, callback) {
  'use strict';

  if (!field.allowed || field.allowed.length === 0) {
    field.allowed = null;
  } else {
    field.allowed = field.allowed.map(function (x) {
      return {
        value: x,
        is_default: x === field['default']
      };
    });
  }

  return function () {
    callback();
  };
};

// Process the fields before applying the template.
var processFields = function (fieldset, callback) {
  'use strict';

  var fields = fieldset.fields;
  var combined;

  combined = fields.reduce(function (acc, field) {
    var retval = acc;

    field.default_exists = field['default'] === '' ? false : field['default'];
    field.is_null = field['default'] === null;
    field.is_false = field['default'] === false;
    field[field.subcategory] = field.subcategory === field.subcategory;

    if (field.docselect || field.docmultiselect) {
      retval = getAllowed(field, acc);
    } else if (field.file) {
      retval = getFileAllowed(field, acc);
    } else if (field.allowed) {
      retval = processAllowed(field, acc);
    }

    return retval;
  }, function () {
    return callback(fieldset);
  });

  combined();

  return fieldset;
};

// Convert field values to an object that can be converted to JSON
var fieldsToObject = function (fields, index) {
  'use strict';

  fields = fields.querySelectorAll('.field-container .field');
  var obj = {
    fields: []
  };

  Array.prototype.forEach.call(fields, function (field, i) {
    var s = store(field);
    var value = getFieldValue(field);
    var instance = s.f('instance');

    obj.fields[i] = {
      id: s.f('field'),
      name: s.f('name'),
      label: s.f('label'),
      head: s.f('head') === 'true',
      reversal: s.f('reversal') === 'true',
      required: s.f('required') === 'true',
      min: dateOrNumber(s.f('subcategory'), s.f('min')),
      max: dateOrNumber(s.f('subcategory'), s.f('max')),
      instance: instance,
      charseq: s.f('charseq'),
      regex: s.f('regex'),
      order: s.f('order') * 1,
      subcategory: s.f('subcategory'),
      value: value
    };

    if (index >= 0) {
      obj.fields[i].index = index;
    }
  });

  return obj;
};

// `min` and `max` are either dates or numbers. Provide the correct
// value or the correct type depending on the subcategory of the field.
dateOrNumber = function (subcategory, fieldvalue) {
  'use strict';

  if (subcategory === 'date') {
    return fieldvalue;
  } else {
    return utils.stringToNumber(fieldvalue);
  }
};

// Get the correct value for a boolean that can be null
var getOpenboolean = function (value) {
  'use strict';

  switch (value) {
  case 'true':
    value = true;
    break;
  case 'false':
    value = false;
    break;
  default:
    value = null;
  }

  return value;
};

// Get a number from a string. Blanks are returned as an empty string.
var getNumber = function (value) {
  'use strict';

  if (utils.isBlank(value)) {
    value = '';
  } else if (!isNaN(value)) {
    value = value * 1;
  }

  return value;
};

// Items in multiple select lists are URL encoded
var getMultiple = function (value) {
  'use strict';

  var retval;

  if (value && value.length > 0) {
    retval = Array.prototype.map.call(value, function (v) {
      return getEncoded(v.value);
    });
  } else {
    retval = null;
  }

  return retval;
};

// Items in select lists are URL encoded
getEncoded = function (value) {
  'use strict';

  return window.decodeURIComponent(value.replace(/\+/g, ' '));
};

// Get the value from a field using the subcategory to ensure
// that the value has the correct type and is properly formatted.
getFieldValue = function (field) {
  'use strict';

  var value;

  switch (store(field).f('subcategory')) {
  case 'boolean':
    value = field.checked;
    break;
  case 'openboolean':
    value = getOpenboolean(field.value);
    break;
  case 'integer':
  case 'rational':
    value = getNumber(field.value);
    break;
  case 'multiselect':
  case 'docmultiselect':
    value = getMultiple(field.selectedOptions);
    break;
  case 'select':
  case 'docselect':
    value = getEncoded(field.value);
    break;
  default:
    value = field.value;
  }

  return value;
};

// Basic initialization of fields.
var initFields = function (container, callback, addInstances) {
  'use strict';

  var url = dpath(container, 'field');
  var allFields = container.querySelectorAll('.fields');
  var section = allFields[allFields.length - 1];
  var prependIt = function (data) {
    if (addInstances) {
      section.id = 'last-added';
    }
    section.insertAdjacentHTML('afterbegin', data);
    if (callback) {
      callback(section);
    }

    editui.afterFreshRefresh(addInstances);
  };

  // This is an ugly bit of callback stuff. This is intended to be
  // rewritten soon.
  var storeIt = function (data) {
    processFields(data, function (processed) {
      var html = templates['fields'](processed);
      sessionStorage.setItem(url, html);
      prependIt(html);
    });
  };

  ifStoredElse(url.toString(), prependIt, storeIt);

  return true;
};

// Initialize and fill multifieldsets.
var fillMultiFieldsets = function (vfieldset) {
  'use strict';

  var id = store(vfieldset).fs('fieldset');
  var container = document.getElementById('container-' + id);
  var url = dpath(vfieldset, 'fieldset');

  container.innerHtml = '';

  Array.prototype.forEach.call(document.querySelectorAll('.multifield'), function (multifield) {
    initFieldset(container, function (fieldset) {
      fillFields(multifield, fieldset);
    });
  });
};

// Initialize and fill normal fieldsets.
var fillNormalFieldsets = function (vfieldset) {
  'use strict';

  fillFields(vfieldset);
};

// Fill the fields with values taken from the view pane.
fillFields = function (container, context) {
  'use strict';

  var saveButton = document.getElementById('save-document-button');

  Array.prototype.forEach.call(document.querySelectorAll('#edit-document-form .ui-state-error'), function (item) {
    item.removeClass('ui-state-error');
  });

  saveButton.classList.remove('hidden');
  saveButton.removeAttribute('disabled');

  Array.prototype.forEach.call(document.querySelectorAll('.field-view'), function (item) {
    var valueJson = item.dataset.fieldValue;
    var id = item.dataset.fieldField;
    var instance = item.dataset.fieldInstance;
    var field;
    var value;

    // TODO: Here is where I could begin making all values be stored as JSON
    if (valueJson) {
      value = JSON.parse(valueJson);
    }

    if (!context) {
      context = document.body;
    }

    field = context.querySelector('.field[data-field-field="' + id + '"]');

    if (field) {
      setFieldValue(field, value, instance);
    }
  });

  return true;
};

// Properly set the value of the field.
setFieldValue = function (field, value, instance) {
  'use strict';

  if (field.classList.contains('boolean')) {
    field.checked = value;
  } else if (value && field.classList.contains('open-boolean')) {
    field.value = value.toString();
  } else {
    field.value = value;
  }

  if (instance && instance.length === 32) {
    field.dataset.fieldInstance = instance;

    editui.setInstanceInfo(field);
  }

  return true;
};

// Exported functions

// Initialize a fieldset.
initFieldset = function (fieldset, callback, addInstances) {
  'use strict';

  var url = dpath(fieldset, 'fieldset').toString();
  var id = store(fieldset).fs('fieldset');
  var container = document.getElementById('container-' + id);
  var appendIt = function (data) {
    container.insertAdjacentHTML('beforeend', data);
    initFields(container, callback, addInstances);
  };
  var storeIt = function (data) {
    var html = templates['fieldset'](data);
    sessionStorage.setItem(url, html);
    appendIt(html);
  };

  ifStoredElse(url, appendIt, storeIt);

  return false;
};

// Before submitting the form, the form data is converted into an object
// that can be serialized to JSON. This begins with the fieldsets.
var fieldsetsToObject = function (root) {
  'use strict';

  var obj = {
    fieldsets: []
  };

  Array.prototype.forEach.call(root.getElementsByTagName('fieldset'), function (fieldset, i) {
    var s = store(fieldset);

    var fields;
    var newFsObj;

    var fsObj = {
      id: s.fs('fieldset'),
      multiple: s.fs('multiple') === 'true',
      collapse: s.fs('collapse') === 'true',
      name: s.fs('name'),
      label: s.fs('label'),
      order: s.fs('order') * 1
    };

    fields = fsContainer(fsObj.id).querySelectorAll('.fields');

    if (!fsObj.multiple) {
      newFsObj = fieldsToObject(fields[0]);
      Array.prototype.forEach.call(Object.keys(newFsObj), function (x) {
        fsObj[x] = newFsObj[x];
      });
    } else {
      fsObj.multifields = [];

      Array.prototype.forEach.call(fields, function (field, j) {
        fsObj.multifields[j] = fieldsToObject(field, j);
      });
    }

    obj.fieldsets[i] = fsObj;
  });

  return obj;
};

// Initialize fieldsets
var initFieldsets = function () {
  'use strict';

  Array.prototype.forEach.call(document.getElementsByTagName('fieldset'), function (fieldset, i) {
    var fs = store(fieldset);

    if (fs.fs('multiple') === 'false') {
      initFieldset(fieldset, false);
    }
  });

  return true;
};

// Remove a multifieldset. This is done after the remove button is
// pressed.
// TODO: Move to editui
var removeFieldset = function (target) {
  'use strict';

  target.parentNode.parentNode.removeChild(target.parentNode);
};

// Fill the fieldset with values from the view pane.
var fillFieldsets = function () {
  'use strict';

  Array.prototype.forEach.call(document.querySelectorAll('.fieldset-view'), function (fieldset) {
    if (store(fieldset).fs('multiple') === 'true') {
      fillMultiFieldsets(fieldset);
    } else {
      fillNormalFieldsets(fieldset);
    }
  });

  editui.afterEditRefresh();

  return true;
};

exports.initFieldset = initFieldset;
exports.fieldsetsToObject = fieldsetsToObject;
exports.initFieldsets = initFieldsets;
exports.removeFieldset = removeFieldset;
exports.fillFieldsets = fillFieldsets;
