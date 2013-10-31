// # The file manager
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Interface for manipulating index conditions.

// Variable Definitions

var initIndexNewDialog = require('./new-dialog.js').initIndexNewDialog;
var initIndexBuilderDialog = require('./builder-dialog.js').initIndexBuilderDialog;
var initReplaceDialog = require('./replace-dialog.js').initReplaceDialog;
var ilistingui = require('./ilistingui.js');
var ipreviewui = require('./ipreviewui.js');
var ihelpers = require('./ihelpers.js');
var ajax = require('../ajax.js');
var flash = require('../flash.js');

// Internal functions

// User interface element
var tableBody = function ()
{
  'use strict';

  return $('#index-conditions-listing tbody');
};

// User interface element
var editingData = function ()
{
  'use strict';

  return $('#index-editing-data');
};

// Make sure the arguments are of the correct type.
var fixArgumentType = function (argument, subcategory, operator)
{
  'use strict';

  switch (subcategory)
  {
  case 'integer':
  case 'rational':
    argument = argument * 1;
    break;
  }

  switch (operator)
  {
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
var getIndexConditions = function (doctypeId, rows)
{
  'use strict';

  var conditions = rows.map(

  function (index, row)
  {
    row = $(row);
    var is_or = row.find('td.or-condition').attr('data-value') === 'true';
    var paren = row.find('td.paren-condition').attr('data-value');
    var condition;

    if (is_or)
    {
      condition = {
        'is_or': true,
        'parens': false
      };
    }
    else if (paren)
    {
      condition = {
        'is_or': false,
        'parens': paren
      };
    }
    else
    {
      var fieldId = row.find('td.field-condition').attr('data-value');
      var fieldsetId = row.find('td.fieldset-condition').attr('data-value');
      var argument = row.find('td.argument-condition').attr('data-value');
      var fieldDoc = ihelpers.getFieldDoc(fieldId, fieldsetId, doctypeId);
      var negate =
        row.find('td.negate-condition').attr('data-value') === 'true';
      var operator = row.find('td.operator-condition').attr('data-value');

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
  }).toArray();

  return conditions;
};

// Initiate the save action.
var saveIndex = function (buttonData, completeFunction)
{
  'use strict';

  var indexId = buttonData.attr('data-index-id');
  var indexRev = buttonData.attr('data-index-rev');
  var url = 'indexes/' + indexId + '?rev=' + indexRev;
  var doctype = buttonData.attr('data-index-doctype');

  var obj = {
    '_id': indexId,
    'category': 'index',
    'doctype': doctype,
    'show_deleted': buttonData.attr('data-index-show_deleted') === 'true',
    'fields': JSON.parse(buttonData.attr('data-index-fields')),
    'fields_label': JSON.parse(buttonData.attr('data-index-fields_label')),
    'name': buttonData.attr('data-index-name'),
    'conditions': getIndexConditions(doctype, $('#index-conditions-listing tbody tr'))
  };

  if (buttonData.attr('data-index-replace_function'))
  {
    obj.replace_function = buttonData.attr('data-index-replace_function');
  }

  ajax.put(url, obj, 'PUT', completeFunction);

  return false;
};

// Initiate the delete action.
var deleteIndex = function (indexId, indexRev, completeMessage, completeFunction)
{
  'use strict';

  var url = 'indexes/' + indexId + '?rev=' + indexRev;
  var title;
  var body;

  $.ajax(
  {
    type: 'DELETE',
    url: url,
    dataType: 'json',
    contentType: 'application/json',
    complete: function (req, status)
    {
      if (req.status === 204)
      {
        title = 'Success';
        body = completeMessage;

        completeFunction();

        flash.highlight(title, body);
      }
      else if (req.status === 409)
      {
        body = JSON.parse(req.responseText);
        title = req.statusText;

        flash.error(title, body.message);
      }
      else if (req.status === 404)
      {
        body = 'Index appears to have been deleted already.';
        title = req.statusText;

        flash.error(title, body);
      }
    }
  });

  return false;
};

// Exported functions

// Initialize the index editing user interface.
var init = function (target)
{
  'use strict';

  var indexId = $(target).attr('data-index-id');
  var url = 'indexes/' + indexId;
  var htmlTarget = $('#index-conditions');

  ajax.legacyHTMLGet(url, function (req)
  {
    htmlTarget.html(req.response);
    tableBody().sortable();
    ipreviewui.get();
  });

  return false;
};

// Save the index.
var save = function ()
{
  'use strict';

  var bData = editingData();

  if (bData.length !== 0)
  {
    var completeFunction = function ()
    {
      init(bData);
      flash.highlight('Success', 'Your index has been saved.');
    };

    saveIndex(bData, completeFunction);
  }
  else
  {
    flash.highlight('Info', 'No index has been chosen to save.');
  }
};

// Open the replace dialog, which allows the user to enter a function
// that will replace the normal output of the index.
var replace = function ()
{
  'use strict';

  var bData = editingData();

  if (bData.length !== 0)
  {
    initReplaceDialog.dialog('open');
  }
  else
  {
    flash.highlight('Info', 'You must choose an index first.');
  }

  return true;
};

// Add a condition using the index builder dialog.
var addCond = function ()
{
  'use strict';

  var bData = editingData();

  if (bData.length !== 0)
  {
    initIndexBuilderDialog(bData.attr('data-index-doctype')).dialog('open');
  }
  else
  {
    flash.highlight('Info', 'You must choose an index first.');
  }

  return true;
};

// Handle the mouse click initiate action of removing a condition.
var remCond = function (target)
{
  'use strict';

  $(target).closest('tr').remove();
  return true;
};

// Open the new index dialog.
var newCond = function ()
{
  'use strict';

  initIndexNewDialog().dialog('open');
  return true;
};

// Delete the current index.
var del = function ()
{
  'use strict';

  var bData = editingData();

  if (bData.length !== 0)
  {
    var indexId = bData.attr('data-index-id');
    var indexRev = bData.attr('data-index-rev');
    var completeMessage = 'Your index has been deleted.';
    var completeFunction = function ()
    {
      $('#index-conditions').empty();
      ilistingui.init();
    };

    if (window.confirm('Are you sure?'))
    {
      deleteIndex(indexId, indexRev, completeMessage, completeFunction);
    }
  }
  else
  {
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
