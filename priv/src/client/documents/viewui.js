// # The view user interface
//
// *Implicit depends:* DOM
//
// View pane UI elements.
//
// *TODO* I may be exporting more than needed.

// Variable Definitions

var templates = require('templates');
var store = require('store').store;
var indexui = require('documents/indexui');
var flash = require('flash');
var ui = require('documents/ui-shared');
var editui = require('./editui.js');
var fieldsets = require('./fieldsets.js');
var ajax = require('ajax');

// Internal functions

// Make an object where fieldsets with deletions are identified.
var getDeletions = function (changes) {
  'use strict';

  return Object.keys(changes).reduce(function (acc, x) {
    // If it was changed and there is no new value, it was deleted.
    if (changes[x].newValue === undefined) {
      if (acc[changes[x].fieldset] === undefined) {
        acc[changes[x].fieldset] = {};
      }
      acc[changes[x].fieldset][x] = changes[x];
    }

    return acc;
  }, {});
};

// Process the document from the server.
var processIncoming = function (docJson, rev) {
  'use strict';

  var withDeletions = {};

  if (docJson.changes) {
    withDeletions = getDeletions(docJson.changes);
  }

  docJson.fieldsets.forEach(function (fset) {
    var fsetId = fset.id;

    if (withDeletions[fsetId] !== undefined) {
      fset.removal = true;
      fset.altered = true;
    }

    var fieldFunc = function (field) {
      var changes = {};
      var change;

      if (docJson.changes) {
        changes = docJson.changes;
      }
      change = changes[field.instance];

      field.json_value = JSON.stringify(field.value);

      if (change !== undefined) {
        field.changed = true;
        fset.altered = true;

        if (change.originalValue === undefined) {
          fset.addition = true;
          field.newfield = true;
        } else {
          field.originalValue = JSON.parse(change.originalValue);
        }
      }

      if (field.subcategory === 'textarea') {
        field.is_textarea = true;
      } else if (field.value && field.subcategory.match('multi')) {
        field.value = field.value.join(', ');
      }

      return true;
    };

    if (fset.multiple) {
      fset.multifields.forEach(function (mfs) {
        mfs.fields.forEach(function (field) {
          fieldFunc(field);
          return true;
        });
      });
    } else {
      fset.fields.forEach(function (field) {
        fieldFunc(field);
        return true;
      });
    }

    return true;
  });

  return true;
};

// Exported functions

// Format the 'update at' and 'created at' timestamps and localize them
// to the current time zone.
var formatTimestamps = function () {
  'use strict';

  Array.prototype.forEach.call(document.querySelectorAll('.timestamp'), function (item) {
    var newDate = (new Date(item.textContent)).toLocaleString();
    if (newDate !== 'Invalid Date') {
      item.textContent = newDate;
    }
  });

  return true;
};

// Get the document.
var get = function (id, rev, callback) {
  'use strict';

  var url = 'documents/' + id;
  var htmlTarget = ui.dv();
  var tmpl;

  if (rev) {
    url = url + '/' + rev;
    htmlTarget = ui.dvt();
    tmpl = function (docJson) {
      return templates['document-view-tree'](docJson);
    };
  } else {
    tmpl = function (docJson) {
      return templates['document-view'](docJson);
    };

  }

  ajax.get(url, function (req) {
    var documentHtml;
    var docJson = req.response;

    processIncoming(docJson, rev);
    documentHtml = tmpl(docJson);
    htmlTarget.innerHTML = documentHtml;
    window.location.hash = id;
    formatTimestamps();
    ui.dv().style.opacity = 1;
    if (callback) {
      callback();
    }

    if (rev) {
      ui.dvt().classList.add('oldrev');
    } else {
      if (store(ui.restoreButton()).d('deleted') === 'true') {
        ui.hideButton(ui.editButton());
        ui.hideButton(ui.deleteButton());
        ui.showButton(ui.restoreButton());
      }
    }
  });

  return true;
};

// Restore the state of a document to that of an earlier revision.
var restore = function (id, rev) {
  'use strict';

  var url = './documents/' + id + '?rev=' + rev;
  var body;
  var title;
  var statusCallbacks = [];
  statusCallbacks[200] = function (req) {
    title = 'Success';
    body = 'Your document was restored.';

    get(id, null, function () {
      ui.dv().style.opacity = 1;
      indexui.get(ui.skey(), ui.sid());
    });
    flash.highlight(title, body);
  };
  var errorCallback = function (req) {
    body = JSON.parse(req.responseText);
    title = req.statusText;

    flash.error(title, body.message);
  };
  statusCallbacks[409] = errorCallback;
  statusCallbacks[404] = errorCallback;

  ajax.del(url, undefined, statusCallbacks);

  return true;
};

// Delete the document.
var del = function (id, rev) {
  'use strict';

  var url = './documents/' + id + '?rev=' + rev;
  var body;
  var title;
  var statusCallbacks = [];
  statusCallbacks[200] = function (req) {
    title = 'Success';
    body = 'Your document was deleted.';

    store(ui.restoreButton()).put('document-rev', req.response.rev);

    ui.hideButton(ui.deleteButton());
    ui.hideButton(ui.editButton());
    ui.showButton(ui.restoreButton());
    ui.dv().style.opacity = 0.5;

    indexui.get(ui.skey(), ui.sid());
    flash.highlight(title, body);
  };
  var errorCallback = function (req) {
    body = JSON.parse(req.responseText);
    title = req.statusText;

    flash.error(title, body.message);
  };
  statusCallbacks[409] = errorCallback;
  statusCallbacks[404] = errorCallback;

  ajax.del(url, undefined, statusCallbacks);

  return true;
};

// Confirm an action.
var confirmIt = function (callback) {
  'use strict';

  if (window.confirm('Are you sure?')) {
    var s = store(ui.viewInfo());
    var id = s.d('document');
    var rev = s.d('rev');

    callback(id, rev);
  }

  return true;
};

// Move the document to the editor.
var edit = function () {
  'use strict';

  editui.clear();
  if (ui.dvt().classList.contains('oldrev')) {
    ui.saveButton().classList.add('oldrev');
  } else {
    ui.saveButton().classList.remove('oldrev');
  }
  fieldsets.fillFieldsets();

  return true;
};

// Ask for confirmation on deletion.
var confirmDelete = function () {
  'use strict';

  var s = store(ui.viewInfo());
  var id = s.d('document');
  var rev = s.d('rev');

  return confirmIt(function () {
    del(id, rev);
  });
};

// Ask for confirmation on restoration.
var confirmRestore = function () {
  'use strict';

  var s = store(ui.viewInfo());
  var id = s.d('document');
  var rev = s.d('rev');

  return confirmIt(function () {
    restore(id, rev);
  });
};

// Expand and collapse elements of the view tree.
var collapseToggle = function (target) {
  'use strict';

  target.parentElement.classList.toggle('collapsed');

  return true;
};

// Get a previous revision.
var fetchRevision = function (target) {
  'use strict';

  var s = store(target);
  var id = s.d('document');
  var oldrev = s.d('oldrev');

  Array.prototype.forEach.call(document.getElementsByClassName('revision-link'), function (item) {
    item.classList.remove('selected-revision');
  });

  target.classList.add('selected-revision');

  get(id, oldrev);

  return true;
};

exports.formatTimestamps = formatTimestamps;
exports.get = get;
exports.restore = restore;
exports.del = del;
exports.confirmIt = confirmIt;
exports.edit = edit;
exports.confirmDelete = confirmDelete;
exports.confirmRestore = confirmRestore;
exports.collapseToggle = collapseToggle;
exports.fetchRevision = fetchRevision;
