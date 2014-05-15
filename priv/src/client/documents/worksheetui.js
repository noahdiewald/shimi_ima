// # The worksheet user interface
//
// *Implicit depends:* DOM, JQuery, globals
// ([application.js](./application.html))
//
// Worksheet pane UI elements.

// Variable Definitions

var Hogan = require('hogan.js');
var templates = require('templates');
var setsui = require('documents/setsui');
var info = require('documents/information');
var ajax = require('ajax');
var flash = require('flash');

// Internal functions

// User interface element
var worksheetsSet = function () {
  'use strict';

  return $('#document-worksheets-set-input');
};

// User interface element
var worksheetsArea = function () {
  'use strict';

  return $('#worksheet-area');
};

// Exported functions

// Select all the visible rows.
var selectAllRows = function (select) {
  'use strict';

  if (select) {
    $('#worksheet-table tbody tr').addClass('selected-row');
    $('#worksheet-table tbody tr input').prop('checked', true);
  } else {
    $('#worksheet-table tbody tr').removeClass('selected-row');
    $('#worksheet-table tbody tr input:checked').prop('checked', false);
  }

  return true;
};

// Set the proper class for a selected row and unset the 'select all'
var rowSelection = function (row, select) {
  'use strict';

  if (select) {
    $('#' + row).addClass('selected-row');
    $('#select-all-worksheet-rows').prop('checked', false);
  } else {
    $('#' + row).removeClass('selected-row');
    $('#select-all-worksheet-rows').prop('checked', false);
  }

  return true;
};

// Select a column.
var columnSelection = function (column, select) {
  'use strict';

  if (select) {
    $('.field-column.' + column).addClass('selected-column');
  } else {
    $('.field-column.' + column).removeClass('selected-column');
  }

  return true;
};

// Show vertical headers for fields and fieldsets.
var showHandles = function () {
  'use strict';

  $('#worksheet-table .handle-column.fieldset').show();

  return true;
};

// Hide vertical headers for fields and fieldsets.
var hideHandles = function () {
  'use strict';

  $('#worksheet-table .handle-column.fieldset').hide();

  return true;
};

// Show the fieldset handle.
var showFieldset = function (fsid) {
  'use strict';

  $('#worksheet-table .handle-column.field.' + fsid).show();

  return true;
};

// Hide the fieldset handle.
var hideFieldset = function (fsid) {
  'use strict';

  $('#worksheet-table .handle-column.field.' + fsid).hide();

  return true;
};

// Show a field.
var showField = function (fid) {
  'use strict';

  $('.field-column.' + fid).show();

  return true;
};

// Hide a field.
var hideField = function (fid) {
  'use strict';

  $('.field-column.' + fid).hide();

  return true;
};

// There are two layers of templating information in the
// template. Activate the second layer.
var buildTemplate = function () {
  'use strict';

  var doctypeInfo = info.info();
  var metaTemp = '{{=<% %>=}}\n' + templates['worksheet'](doctypeInfo);
  globals[info.worksheetName()] = Hogan.compile(metaTemp);

  return true;
};

// Render the worksheet.
var fillWorksheet = function () {
  'use strict';

  var setName = worksheetsSet().val();
  var url = 'worksheets';
  var complete = function (req) {
    var ws = globals[info.worksheetName()].render(req.response);
    worksheetsArea().html(ws);
  };

  if (!setName.isBlank()) {
    var thisSet = setsui.getSet(setName)[1];

    if (thisSet.length <= 250) {
      var setIds = thisSet.map(function (x) {
        return x[1];
      });

      ajax.post(url, setIds, complete);
    } else {
      flash.error('Could not load worksheet', 'the current set size is limited to 250 items.');
    }
  }

  return true;
};

exports.selectAllRows = selectAllRows;
exports.rowSelection = rowSelection;
exports.columnSelection = columnSelection;
exports.showHandles = showHandles;
exports.hideHandles = hideHandles;
exports.showFieldset = showFieldset;
exports.hideFieldset = hideFieldset;
exports.showField = showField;
exports.hideField = hideField;
exports.buildTemplate = buildTemplate;
exports.fillWorksheet = fillWorksheet;
