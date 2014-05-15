// # Documents sub-application
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Edit pane UI elements

// Variable Definitions

var templates = require('templates');
var store = require('store').store;
var form = require('form');
var flash = require('flash');
var ajax = require('ajax');
var fieldsets = require('./fieldsets.js');
var viewui = require('documents/viewui');
var indexui = require('documents/indexui');
var info = require('documents/information');
var ui = require('documents/ui-shared');
var uuid = require('node-uuid');
var afterRefresh;
var setInstanceInfo;

// Internal functions

// Get the fieldset id for a field id.
var getFieldsetId = function (fieldId) {
  'use strict';

  var lookup = JSON.parse(sessionStorage.getItem(ui.identifier() + '_fieldsToFieldset'));

  return lookup[fieldId];
};

// Display validation error properly.
var validationError = function (req) {
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

// The expander for textareas may need the proper information set for
// multiple fieldsets
var setExpander = function (item) {
  'use strict';

  var expander = item.parentElement.querySelector('.expander');

  if (expander) {
    expander.dataset.groupId = item.id;
  }

  return true;
};

// Fields need to have instances. This should ensure they have them.
var instances = function (addInstances) {
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

// Exported functions

// Initialize the editing pane.
// TODO: refactor taking advantage of information.info(). Old code used
// ajax calls and server rendered HTML.
var init = function () {
  'use strict';

  var fs = {};
  var editArea = document.getElementById('document-edit');

  fs.fieldsets = info.info().fieldsets;
  fs.has_rows = fs.fieldsets ? (fs.fieldsets.length > 0) : false;
  editArea.innerHTML = templates['document-edit'](fs);
  // TODO: replace tabs functionality.
  $('#edit-tabs').tabs();
  fieldsets.initFieldsets();

  return true;
};

// Focus on the first focusable input element in an active tab.
var selectInput = function () {
  'use strict';

  var inputable = 'input, select, textarea';
  var curId = document.querySelector('.ui-tabs-active a').getAttribute('href').slice(1, 33);

  document.getElementById(curId).querySelector(inputable).focus();

  return true;
};

// Used as a variation of `afterRefresh` where a boolean is provided
// to specify if new instances identifiers should be created and
// set. Basically this is for a completely fresh refresh, when the form
// is in the state such that a document can be created but no information
// is available to do an update.
var afterFreshRefresh = function (addInstances) {
  'use strict';

  afterRefresh(addInstances);

  return true;
};

// Run after the edit button in the view UI is clicked.
var afterEditRefresh = function () {
  'use strict';

  var sharedAttrs = ['data-document-id', 'data-document-rev'];

  sharedAttrs.forEach(function (elem) {
    ui.saveButton().setAttribute(elem, ui.editButton().getAttribute(elem));
  });

  ui.showButton(ui.saveButton());
  afterRefresh();

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
var clearErrorStates = function () {
  'use strict';

  Array.prototype.forEach.call(ui.editForm().querySelectorAll('.ui-state-error'), function (item) {
    item.classList.remove('ui-state-error');
  });

  return true;
};

// Remove all the fields.
var removeFields = function () {
  'use strict';

  Array.prototype.forEach.call(document.querySelectorAll('.fields'), function (item) {
    item.parentNode.removeChild(item);
  });
};

// Combine two shallow objects.
var extend = function (oldO, newO) {
  'use strict';

  Array.prototype.forEach.call(Object.keys(newO), function (key) {
    oldO[key] = newO[key];
  });

  return oldO;
};

// To be run if the user chooses to save the form contents. This is an
// update, not creation.
var save = function () {
  'use strict';

  if (ui.saveButton().classList.contains('oldrev')) {
    if (!window.confirm('This data is from an older version of this document. Are you sure you want to restore it?')) {
      return false;
    }
  }

  var body;
  var title;
  var s = store(ui.saveButton());
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
  var success = function () {
    title = 'Success';
    body = 'Your document was saved.';
    viewui.get(doc);
    indexui.get(ui.skey(), ui.sid());
    flash.highlight(title, body);
    ui.saveButton().classList.remove('oldrev');
    ui.showButton(ui.saveButton());
  };
  statusCallbacks[204] = success;
  statusCallbacks[200] = success;
  statusCallbacks[403] = function (req) {
    validationError(req);
    ui.showButton(ui.saveButton());
  };
  statusCallbacks[409] = function (req) {
    body = JSON.parse(req.responseText);
    title = req.statusText;

    flash.error(title, body.message);
    ui.hideButton(ui.saveButton());
  };

  clearErrorStates();
  ui.hideButton(ui.saveButton());
  newObj = fieldsets.fieldsetsToObject(ui.editForm());
  obj = extend(obj, newObj);
  ajax.put(url, obj, undefined, statusCallbacks);
};

// To be run if creating a new document.
var create = function () {
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

    ui.hideButton(ui.saveButton());
    removeFields();
    fieldsets.initFieldsets();
    viewui.get(documentId);
    indexui.get(ui.skey(), ui.sid());
    flash.highlight(title, body);
    ui.showButton(ui.createButton());
  };
  statusCallbacks[403] = function (req) {
    validationError(req);
    ui.showButton(ui.createButton());
  };

  clearErrorStates();
  ui.hideButton(ui.createButton());
  newObj = fieldsets.fieldsetsToObject(ui.editForm());
  obj = extend(obj, newObj);
  ajax.post(url, obj, undefined, statusCallbacks);
};

// Clear the form.
var clear = function () {
  'use strict';

  clearErrorStates();
  ui.hideButton(ui.saveButton());
  removeFields();
  fieldsets.initFieldsets();
};

// Display a help dialog for a form field.
var showHelpDialog = function (target) {
  'use strict';

  if (target.classList.contains('.label-text')) {
    target = target.parentElement.querySelector('.ui-icon-help');
  }

  // TODO: remove this JQuery UI dependency
  $('#help-dialog').dialog().dialog('open').find('#help-dialog-text').html(target.getAttribute('title'));

  return true;
};

// Contract and expand textarea elements.
var toggleTextarea = function (target) {
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

// When the item has an instance, the id and group id must be reset.
setInstanceInfo = function (item) {
  'use strict';

  item.id = item.dataset.fieldField + '-' + item.dataset.fieldInstance;
  item.dataset.groupId = item.id;
  setExpander(item);
};

exports.init = init;
exports.selectInput = selectInput;
exports.afterFreshRefresh = afterFreshRefresh;
exports.afterEditRefresh = afterEditRefresh;
exports.afterRefresh = afterRefresh;
exports.save = save;
exports.create = create;
exports.clear = clear;
exports.toggleTextarea = toggleTextarea;
exports.setInstanceInfo = setInstanceInfo;
exports.showHelpDialog = showHelpDialog;
