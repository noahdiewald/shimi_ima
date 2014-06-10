// # Documents sub-application
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Edit pane UI elements

// ## Variable Definitions

// ### Imported Modules

var ajax = require('ajax');
var flash = require('flash');
var form = require('form');
var indexui = require('documents/indexui');
var info = require('documents/information');
var path = require('../path.js').path;
var store = require('store').store;
var templates = require('templates');
var ui = require('documents/ui-shared');
var utils = require('utils');
var uuid = require('node-uuid');
var viewui = require('documents/viewui');

// ### External Function Names

var clear;
var create;
var fillFieldsets;
var initFieldset;
var init;
var removeFieldset;
var save;
var selectInput;
var showHelpDialog;
var toggleTextarea;

// ### Internal Function Names

var afterEditRefresh;
var afterFreshRefresh;
var afterRefresh;
var clearErrorStates;
var dateOrNumber;
var dpath;
var extend;
var fieldsetsToObject;
var fieldsToObject;
var fillFields;
var fillMultiFieldsets;
var fillNormalFieldsets;
var fsContainer;
var getAllowed;
var getEncoded;
var getFieldsetId;
var getFieldValue;
var getFileAllowed;
var getMultiple;
var getNumber;
var getOpenboolean;
var ifStoredElse;
var initFields;
var initFieldsets;
var instances;
var processAllowed;
var processFields;
var removeFields;
var setExpander;
var setFieldValue;
var setInstanceInfo;
var validationError;

// ## Internal functions

// Run after the edit button in the view UI is clicked.
afterEditRefresh = function () {
  'use strict';

  var sharedAttrs = ['data-document-document', 'data-document-rev'];

  sharedAttrs.forEach(function (elem) {
    ui.saveButton().setAttribute(elem, ui.viewInfo().getAttribute(elem));
  });

  ui.showEnable(ui.saveButton());
  afterRefresh();

  return true;
};

// Used as a variation of `afterRefresh` where a boolean is provided
// to specify if new instances identifiers should be created and
// set. Basically this is for a completely fresh refresh, when the form
// is in the state such that a document can be created but no information
// is available to do an update.
afterFreshRefresh = function (addInstances) {
  'use strict';

  afterRefresh(addInstances);

  return true;
};

// Essentially initialization of the form. If `addInstances` is true,
// new instance identifiers will be created for a blank form.
afterRefresh = function (addInstances) {
  'use strict';

  instances(addInstances);
  form.initDateFields();

  return true;
};

// Remove a class from some items.
clearErrorStates = function () {
  'use strict';

  Array.prototype.forEach.call(ui.editForm().querySelectorAll('.ui-state-error'), function (item) {
    item.classList.remove('ui-state-error');
  });

  return true;
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

// Get the doctype path.
dpath = function (source, category) {
  'use strict';

  var url = path(source, category);
  url.doctype = false;
  return url;
};

// Combine two shallow objects.
extend = function (oldO, newO) {
  'use strict';

  Array.prototype.forEach.call(Object.keys(newO), function (key) {
    oldO[key] = newO[key];
  });

  return oldO;
};

// Before submitting the form, the form data is converted into an object
// that can be serialized to JSON. This begins with the fieldsets.
fieldsetsToObject = function (root) {
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

// Convert field values to an object that can be converted to JSON
fieldsToObject = function (fields, index) {
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

// Fill the fields with values taken from the view pane.
fillFields = function (container, context) {
  'use strict';

  if (!context) {
    context = document.body;
  }

  clearErrorStates();
  ui.showEnable(ui.saveButton());

  Array.prototype.forEach.call(container.querySelectorAll('.field-view'), function (item) {
    var valueJson = item.dataset.fieldValue;
    var id = item.dataset.fieldField;
    var instance = item.dataset.fieldInstance;
    var field;
    var value;

    // TODO: Here is where I could begin making all values be stored as JSON
    if (valueJson) {
      value = JSON.parse(valueJson);
    }

    field = context.querySelector('.field[data-field-field="' + id + '"]');

    if (field) {
      setFieldValue(field, value, instance);
    }
  });

  return true;
};

// Initialize and fill multifieldsets.
fillMultiFieldsets = function (vfieldset) {
  'use strict';

  var id = store(vfieldset).fs('fieldset');
  var container = fsContainer(id);
  var url = dpath(vfieldset, 'fieldset');

  container.innerHtml = '';

  Array.prototype.forEach.call(document.querySelectorAll('.multifield'), function (multifield) {
    initFieldset(container, function (fieldset) {
      fillFields(multifield, fieldset);
    });
  });
};

// Initialize and fill normal fieldsets.
fillNormalFieldsets = function (vfieldset) {
  'use strict';

  fillFields(vfieldset);
};

// Get the container for a fieldset with `id`.
fsContainer = function (id) {
  'use strict';

  return document.getElementById('container-' + id);
};

// Get the allowed doc values from the server.
getAllowed = function (field, callback) {
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

// Items in select lists are URL encoded
getEncoded = function (value) {
  'use strict';

  return window.decodeURIComponent(value.replace(/\+/g, ' '));
};

// Get the fieldset id for a field id.
getFieldsetId = function (fieldId) {
  'use strict';

  var lookup = JSON.parse(sessionStorage.getItem(ui.identifier() + '_fieldsToFieldset'));

  return lookup[fieldId];
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

// Get the allowed file values from the server.
getFileAllowed = function (field, callback) {
  'use strict';

  // This is unimplemented.
  return function () {
    callback();
  };
};

// Items in multiple select lists are URL encoded
getMultiple = function (value) {
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

// Get a number from a string. Blanks are returned as an empty string.
getNumber = function (value) {
  'use strict';

  if (utils.isBlank(value)) {
    value = '';
  } else if (!isNaN(value)) {
    value = value * 1;
  }

  return value;
};

// Get the correct value for a boolean that can be null
getOpenboolean = function (value) {
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

// If the item referred to by `key` is in session storage perform the
// `success` action with the stored items as the argument, otherwise,
// get the item from the document info and render it.
// WARNING: This exists as part of a minimal change to support a very
// different way of managing this information. It will be rewritten
// soon.
ifStoredElse = function (key, success, otherwise) {
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

// Basic initialization of fields.
initFields = function (container, callback, addInstances) {
  'use strict';

  var url = dpath(container, 'field');
  var allFields = container.querySelectorAll('.fields');
  var section = allFields[allFields.length - 1];
  var prependIt = function (data) {
    if (addInstances) {
      section.id = 'last-added';
    }
    section.insertAdjacentHTML('afterbegin', data);
    // The purpose of the callback is generally to fill the fields using
    // a function like `fillFields`.
    if (callback) {
      callback(section);
    }

    afterFreshRefresh(addInstances);
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

// Initialize fieldsets
initFieldsets = function () {
  'use strict';

  Array.prototype.forEach.call(document.getElementsByTagName('fieldset'), function (fieldset, i) {
    var fs = store(fieldset);

    if (fs.fs('multiple') === 'false') {
      initFieldset(fieldset, false);
    }
  });

  return true;
};

// Fields need to have instances. This should ensure they have them.
instances = function (addInstances) {
  'use strict';

  var makeInstance = function () {
    return uuid.v4().replace(/-/g, '');
  };

  Array.prototype.forEach.call(document.querySelectorAll('#last-added [data-field-instance]'), function (item) {
    if (!item.dataset.fieldInstance || item.dataset.fieldInstance.length === '') {
      var instance = makeInstance();

      item.dataset.fieldInstance = instance;
      setInstanceInfo(item);
    }
  });

  return true;
};

// Process the listing of allowed values.
processAllowed = function (field, callback) {
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
processFields = function (fieldset, callback) {
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

// Remove all the fields.
removeFields = function () {
  'use strict';

  Array.prototype.forEach.call(document.querySelectorAll('.fields'), function (item) {
    item.parentNode.removeChild(item);
  });
};

// The expander for textareas may need the proper information set for
// multiple fieldsets
setExpander = function (item) {
  'use strict';

  var expander = item.parentElement.querySelector('.expander');

  if (expander) {
    expander.dataset.groupId = item.id;
  }

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

    setInstanceInfo(field);
  }

  return true;
};

// When the item has an instance, the id and group id must be reset.
setInstanceInfo = function (item) {
  'use strict';

  item.id = item.dataset.fieldField + '-' + item.dataset.fieldInstance;
  item.dataset.groupId = item.id;
  setExpander(item);
};

// Display validation error properly.
validationError = function (req) {
  'use strict';

  var body = JSON.parse(req.responseText);
  var title = req.statusText;

  var invalid = document.querySelector('[data-field-instance="' + body.instance + '"]');
  var invalidTab = document.querySelector('[href="#' + getFieldsetId(invalid.dataset.fieldField) + '"]').parentElement;

  invalidTab.classList.add('ui-state-error');
  invalid.classList.add('ui-state-error');

  flash.error(title, body.fieldname + ' ' + body.message);

  return true;
};

// ## Exported functions

// Clear the form.
clear = function () {
  'use strict';

  clearErrorStates();
  ui.hideDisable(ui.saveButton());
  removeFields();
  initFieldsets();
};

// To be run if creating a new document.
create = function () {
  'use strict';

  var s = store(ui.createButton());
  var url = 'documents';
  var newObj;
  var obj = {
    doctype: s.d('doctype'),
    description: s.d('description')
  };
  var statusCallbacks = [];
  statusCallbacks[201] = function (req) {
    var title = 'Success';
    var body = 'Your document was created.';
    var documentId = req.getResponseHeader('Location').match(/[a-z0-9]*$/);

    ui.hideDisable(ui.saveButton());
    removeFields();
    initFieldsets();
    viewui.get(documentId);
    indexui.get(ui.skey(), ui.sid());
    flash.highlight(title, body);
    ui.showEnable(ui.createButton());
  };
  statusCallbacks[403] = function (req) {
    validationError(req);
    ui.showEnable(ui.createButton());
  };

  clearErrorStates();
  ui.hideDisable(ui.createButton());
  newObj = fieldsetsToObject(ui.editForm());
  obj = extend(obj, newObj);
  ajax.post(url, obj, undefined, statusCallbacks);
};

// Fill the fieldset with values from the view pane.
fillFieldsets = function () {
  'use strict';

  Array.prototype.forEach.call(document.querySelectorAll('.fieldset-view'), function (fieldset) {
    if (store(fieldset).fs('multiple') === 'true') {
      fillMultiFieldsets(fieldset);
    } else {
      fillNormalFieldsets(fieldset);
    }
  });

  afterEditRefresh();

  return true;
};

// Initialize a fieldset.
initFieldset = function (fieldset, callback, addInstances) {
  'use strict';

  var url = dpath(fieldset, 'fieldset').toString();
  var id = store(fieldset).fs('fieldset');
  var container = fsContainer(id);
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

// Initialize the editing pane.
// TODO: refactor taking advantage of information.info(). Old code used
// ajax calls and server rendered HTML.
init = function () {
  'use strict';

  var fs = {};
  var editArea = document.getElementById('document-edit');

  fs.fieldsets = info.info().fieldsets;
  fs.has_rows = fs.fieldsets ? (fs.fieldsets.length > 0) : false;
  editArea.innerHTML = templates['document-edit'](fs);
  // TODO: replace tabs functionality.
  $('#edit-tabs').tabs();
  initFieldsets();

  return true;
};

// Remove a multifieldset. This is done after the remove button is
// pressed.
removeFieldset = function (target) {
  'use strict';

  target.parentNode.parentNode.removeChild(target.parentNode);
};

// To be run if the user chooses to save the form contents. This is an
// update, not creation.
save = function () {
  'use strict';

  if (ui.saveButton().classList.contains('oldrev')) {
    if (!window.confirm('This data is from an older version of this document. Are you sure you want to restore it?')) {
      return false;
    }
  }

  var sb = ui.saveButton();
  var s = store(sb);
  var doc = s.d('document');
  var rev = s.d('rev');
  var url = './documents/' + doc + '?rev=' + rev;
  var firstIndex = document.getElementById('first-index-element');
  var newObj;
  var obj = {
    doctype: s.d('doctype'),
    description: s.d('description')
  };
  var statusCallbacks = [];
  var success = function (req) {
    viewui.get(doc);
    indexui.get(ui.skey(), ui.sid());
    flash.highlight('Success', 'Your document was saved.');
    sb.classList.remove('oldrev');
    sb.dataset.documentRev = req.response.rev;
    ui.showEnable(sb);
  };
  statusCallbacks[204] = success;
  statusCallbacks[200] = success;
  statusCallbacks[403] = function (req) {
    validationError(req);
    ui.showEnable(sb);
  };
  statusCallbacks[409] = function (req) {
    flash.error(req.statusText, req.response.message);
    ui.hideDisable(sb);
  };

  clearErrorStates();
  ui.hideDisable(ui.saveButton());
  newObj = fieldsetsToObject(ui.editForm());
  obj = extend(obj, newObj);
  ajax.put(url, obj, undefined, statusCallbacks);
};

// Focus on the first focusable input element in an active tab.
selectInput = function () {
  'use strict';

  var inputable = 'input, select, textarea';
  var curId = document.querySelector('.ui-tabs-active a').getAttribute('href').slice(1, 33);

  document.getElementById(curId).querySelector(inputable).focus();

  return true;
};

// Display a help dialog for a form field.
showHelpDialog = function (target) {
  'use strict';

  if (target.classList.contains('.label-text')) {
    target = target.parentElement.querySelector('.ui-icon-help');
  }

  // TODO: remove this JQuery UI dependency
  $('#help-dialog').dialog().dialog('open').find('#help-dialog-text').html(target.getAttribute('title'));

  return true;
};

// Contract and expand textarea elements.
toggleTextarea = function (target) {
  'use strict';

  var textarea = document.getElementById(target.dataset.groupId);

  if (target.id === textarea.dataset.groupId) {
    // This is the key sequence case.
    textarea.classList.toggle('expanded');
    textarea.parentElement.querySelector('span.expander').classList.toggle('expanded');
  } else {
    // This is the click case.
    textarea.classList.toggle('expanded');
    target.classList.toggle('expanded');
  }

  return true;
};

exports.clear = clear;
exports.create = create;
exports.fillFieldsets = fillFieldsets;
exports.initFieldset = initFieldset;
exports.init = init;
exports.removeFieldset = removeFieldset;
exports.save = save;
exports.selectInput = selectInput;
exports.showHelpDialog = showHelpDialog;
exports.toggleTextarea = toggleTextarea;
