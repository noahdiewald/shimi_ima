// # The file manager
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Interface for manipulating index conditions.

// Variable Definitions

var initIndexNewDialog = require('index_tool/new-dialog').initIndexNewDialog;
var initIndexBuilderDialog = require('index_tool/builder-dialog').initIndexBuilderDialog;
var initReplaceDialog = require('index_tool/replace-dialog').initReplaceDialog;
var ilistingui = require('index_tool/ilistingui');
var ipreviewui = require('index_tool/ipreviewui');
var ihelpers = require('index_tool/ihelpers');
var ajax = require('ajax');
var flash = require('flash');

// Internal functions

// User interface element
var tableBody = function () {
  'use strict';

  return document.getElementById('index-conditions-listing').getElementByTagName('tbody');

};

// User interface element
var editingData = function () {
  'use strict';

  return document.getElementById('index-editing-data');
};

// Make sure the arguments are of the correct type.
var fixArgumentType = function (argument, subcategory, operator) {
  'use strict';

  switch (subcategory) {
  case 'integer':
  case 'rational':
    argument = argument * 1;
    break;
  }

  switch (operator) {
  case 'hasExactly':
  case 'hasGreater':
  case 'hasLess':
    argument = Math.floor(argument * 1);
    break;
  }

  return argument;
};

// Use data in `data` attributes of HTML elements to produce an array
// of conditions.
var getIndexConditions = function (doctypeId, rows) {
  'use strict';

  var conditions = Array.prototype.map.call(rows, function (row) {
    var is_or = row.querySelector('td.or-condition').dataset.value === 'true';
    var paren = row.querySelector('td.paren-condition').dataset.value;
    var condition;

    if (is_or) {
      condition = {
        'is_or': true,
        'parens': false
      };
    } else if (paren) {
      condition = {
        'is_or': false,
        'parens': paren
      };
    } else {
      var fieldId = row.querySelector('td.field-condition').dataset.value;
      var fieldsetId = row.querySelector('td.fieldset-condition').dataset.value;
      var argument = row.querySelector('td.argument-condition').dataset.value;
      var fieldDoc = ihelpers.getFieldDoc(fieldId, fieldsetId, doctypeId);
      var negate = row.querySelector('td.negate-condition').dataset.value === 'true';
      var operator = row.querySelector('td.operator-condition').dataset.value;

      argument = fixArgumentType(argument, fieldDoc.subcategory, operator);

      condition = {
        'is_or': false,
        'parens': false,
        'negate': negate,
        'fieldset': fieldsetId,
        'field': fieldId,
        'operator': operator,
        'argument': argument
      };
    }

    return condition;
  });

  return conditions;
};

// Initiate the save action.
var saveIndex = function (buttonData, completeFunction) {
  'use strict';

  var indexId = buttonData.dataset.indexId;
  var indexRev = buttonData.dataset.indexRev;
  var url = 'indexes/' + indexId + '?rev=' + indexRev;
  var doctype = buttonData.dataset.indexDoctype;

  var obj = {
    '_id': indexId,
    'category': 'index',
    'doctype': doctype,
    'show_deleted': buttonData.dataset.indexShow_deleted === 'true',
    'fields': JSON.parse(buttonData.dataset.indexFields),
    'fields_label': JSON.parse(buttonData.dataset.indexFields_label),
    'name': buttonData.dataset.indexName,
    'conditions': getIndexConditions(doctype, document.querySelectorAll('#index-conditions-listing tbody tr'))
  };

  if (buttonData.dataset.indexReplace_function) {
    obj.replace_function = buttonData.dataset.indexReplace_function;
  }

  ajax.put(url, obj, 'PUT', completeFunction);

  return false;
};

// Initiate the delete action.
var deleteIndex = function (indexId, indexRev, completeMessage, completeFunction) {
  'use strict';

  var url = 'indexes/' + indexId + '?rev=' + indexRev;
  var title;
  var body;
  var statusCallbacks = [];

  statusCallbacks[204] = function () {
    title = 'Success';
    body = completeMessage;

    completeFunction();

    flash.highlight(title, body);
  };
  statusCallbacks[409] = function (req) {
    body = req.response.message;
    title = req.statusText;

    flash.error(title, body);
  };
  statusCallbacks[404] = function (req) {
    body = 'Index appears to have been deleted already.';
    title = req.statusText;

    flash.error(title, body);
  };

  ajax.del(url, undefined, statusCallbacks);

  return false;
};

// Exported functions

// Initialize the index editing user interface.
var init = function (target) {
  'use strict';

  var indexId = target.dataset.indexId;
  var url = 'indexes/' + indexId;
  var htmlTarget = document.getElementById('index-conditions');

  ajax.get(url, function (req) {
    htmlTarget.innerHTML = templates['index-conditions'](req.response);
    $(tableBody()).sortable();
    ipreviewui.get();
  });

  return false;
};

// Save the index.
var save = function () {
  'use strict';

  var bData = editingData();

  if (bData.length !== 0) {
    var completeFunction = function () {
      init(bData);
      flash.highlight('Success', 'Your index has been saved.');
    };

    saveIndex(bData, completeFunction);
  } else {
    flash.highlight('Info', 'No index has been chosen to save.');
  }
};

// Open the replace dialog, which allows the user to enter a function
// that will replace the normal output of the index.
var replace = function () {
  'use strict';

  var bData = editingData();

  if (bData.length !== 0) {
    initReplaceDialog.dialog('open');
  } else {
    flash.highlight('Info', 'You must choose an index first.');
  }

  return true;
};

// Add a condition using the index builder dialog.
var addCond = function () {
  'use strict';

  var bData = editingData();

  if (bData.length !== 0) {
    initIndexBuilderDialog(bData.dataset.indexDoctype).dialog('open');
  } else {
    flash.highlight('Info', 'You must choose an index first.');
  }

  return true;
};

// Handle the mouse click initiate action of removing a condition.
var remCond = function (target) {
  'use strict';

  //$(target).closest('tr').remove();
  throw 'intentional error';
  return true;
};

// Open the new index dialog.
var newCond = function () {
  'use strict';

  initIndexNewDialog().dialog('open');
  return true;
};

// Delete the current index.
var del = function () {
  'use strict';

  var bData = editingData();

  if (bData.length !== 0) {
    var indexId = bData.dataset.indexId;
    var indexRev = bData.dataset.indexRev;
    var completeMessage = 'Your index has been deleted.';
    var completeFunction = function () {
      document.getElementById('index-conditions').innerHTML = '';
      ilistingui.init();
    };

    if (window.confirm('Are you sure?')) {
      deleteIndex(indexId, indexRev, completeMessage, completeFunction);
    }
  } else {
    flash.highlight('Info', 'No index has been chosen to delete.');
  }

  return true;
};

exports.init = init;
exports.save = save;
exports.replace = replace;
exports.addCond = addCond;
exports.remCond = remCond;
exports.newCond = newCond;
exports.del = del;
