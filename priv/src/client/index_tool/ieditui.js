// # The Index Condition Editor
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
var templates = require('templates');
var ajax = require('ajax');
var flash = require('flash');

var saveIndex2;

// Internal functions

// User interface element
var tableBody = function () {
  'use strict';

  return document.getElementById('index-conditions-listing').getElementsByTagName('tbody')[0];

};

// User interface element
var editingData = function () {
  'use strict';

  return document.getElementById('index-editing-data');
};

// User interface element
var indexConditions = function () {
  'use strict';

  return document.querySelectorAll('#index-conditions-listing tbody tr');
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

var getRowValue = function (row, rowType) {
  'use strict';

  var retval;
  var possibleRow = row.querySelector('td.' + rowType + '-condition');

  if (possibleRow) {
    retval = possibleRow.dataset.value;
  } else {
    retval = false;
  }

  return retval;
};

// Use data in `data` attributes of HTML elements to produce an array
// of conditions.
var getIndexConditions = function (doctypeId, rows) {
  'use strict';

  var conditions = Array.prototype.map.call(rows, function (row) {
    var is_or = getRowValue(row, 'or') === 'true';
    var paren = getRowValue(row, 'paren');
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
      var negate = row.querySelector('td.negate-condition').dataset.value === 'true';
      var operator = row.querySelector('td.operator-condition').dataset.value;
      var fieldDoc;

      if (fieldsetId !== 'metadata') {
        fieldDoc = ihelpers.getFieldDoc(fieldId, fieldsetId, doctypeId);
        argument = fixArgumentType(argument, fieldDoc.subcategory, operator);
      }

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

// Make sure the fieldocs are available. This, like a number of other
// quick fixes found here is super hacky.
var getFieldDocs = function (doctypeId, rows, bd, cf) {
  'use strict';

  Array.prototype.forEach.call(rows, function (row, index) {
    var is_or = getRowValue(row, 'or') === 'true';
    var paren = getRowValue(row, 'paren');
    var condition;

    if (is_or) {
      return true;
    } else if (paren) {
      return true;
    } else {
      var fieldId = row.querySelector('td.field-condition').dataset.value;
      var fieldsetId = row.querySelector('td.fieldset-condition').dataset.value;
      var fieldDoc;

      if (fieldsetId !== 'metadata' && index !== (rows.length - 1)) {
        ihelpers.getFieldDoc(fieldId, fieldsetId, doctypeId);
      } else if (index === (rows.length - 1)) {
        ihelpers.getFieldDoc(fieldId, fieldsetId, doctypeId, function () {
          saveIndex2(bd, cf);

          return false;
        });
      }

      return false;
    }

    return false;
  });

  return false;
};

// Preload the field data.
var saveIndex = function (buttonData, completeFunction) {
  'use strict';

  var doctype = buttonData.dataset.indexDoctype;

  getFieldDocs(doctype, indexConditions(), buttonData, completeFunction);

  return false;
};

// Perform the save action.
saveIndex2 = function (buttonData, completeFunction) {
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
    'fields': buttonData.dataset.indexFields,
    'fields_label': buttonData.dataset.indexFields_label,
    'name': buttonData.dataset.indexName,
    'conditions': getIndexConditions(doctype, indexConditions())
  };

  if (buttonData.dataset.indexReplace_function) {
    obj.replace_function = buttonData.dataset.indexReplace_function;
  }

  ajax.put(url, obj, completeFunction);

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

var processConditions = function (obj) {
  'use strict';

  var conditions = obj.conditions;

  conditions.map(function (condition) {
    var parenVal = condition.parens;

    if (parenVal) {
      condition['paren_' + parenVal] = true;
    }

    return condition;
  });

  return obj;
};

// Exported functions

// Initialize the index editing user interface.
var init = function (target) {
  'use strict';

  var indexId = target.dataset.indexId;
  var url = 'indexes/' + indexId;
  var htmlTarget = document.getElementById('index-conditions');

  ajax.get(url, function (req) {
    var indexData = processConditions(req.response);
    htmlTarget.innerHTML = templates['index-conditions'](indexData);
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

  tableBody().removeChild(target.parentElement.parentElement);

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
