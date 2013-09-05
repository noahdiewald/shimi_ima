// # The worksheet user interface
//
// *Implicit depends:* DOM, JQuery, Hogan, templates, shimi.globals
// ([application.js](./application.html))
//
// Worksheet pane UI elements.

// Variable Definitions

var setsui = require('./setsui.js');
var documents = require('./documents.js');
var sets = require('../form.js');
var flash = require('../flash.js');

// Internal functions

// User interface element
var worksheetsSet = function ()
{
  return $('#document-worksheets-set-input');
};

// User interface element
var worksheetsArea = function ()
{
  return $('#worksheet-area');
};

// Name for the worksheet template.
var worksheetName = function ()
{
  return documents.identifier() + '_worksheet-template';
};

// Exported functions

// Select all the visible rows.
var selectAllRows = function (select)
{
  if (select)
  {
    $('#worksheet-table tbody tr').addClass('selected-row');
    $('#worksheet-table tbody tr input').prop('checked', true);
  }
  else
  {
    $('#worksheet-table tbody tr').removeClass('selected-row');
    $('#worksheet-table tbody tr input:checked').prop('checked', false);
  }

  return true;
};

// Set the proper class for a selected row and unset the 'select all'
var rowSelection = function (row, select)
{
  if (select)
  {
    $('#' + row).addClass('selected-row');
    $('#select-all-worksheet-rows').prop('checked', false);
  }
  else
  {
    $('#' + row).removeClass('selected-row');
    $('#select-all-worksheet-rows').prop('checked', false);
  }

  return true;
};

// Select a column.
var columnSelection = function (column, select)
{
  if (select)
  {
    $('.field-column.' + column).addClass('selected-column');
  }
  else
  {
    $('.field-column.' + column).removeClass('selected-column');
  }

  return true;
};

// Show vertical headers for fields and fieldsets.
var showHandles = function ()
{
  $('#worksheet-table .handle-column.fieldset').show();

  return true;
};

// Hide vertical headers for fields and fieldsets.
var hideHandles = function ()
{
  $('#worksheet-table .handle-column.fieldset').hide();

  return true;
};

// Show the fieldset handle.
var showFieldset = function (fsid)
{
  $('#worksheet-table .handle-column.field.' + fsid).show();

  return true;
};

// Hide the fieldset handle.
var hideFieldset = function (fsid)
{
  $('#worksheet-table .handle-column.field.' + fsid).hide();

  return true;
};

// Show a field.
var showField = function (fid)
{
  $('.field-column.' + fid).show();

  return true;
};

// Hide a field.
var hideField = function (fid)
{
  $('.field-column.' + fid).hide();

  return true;
};

// There are two layers of templating information in the
// template. Activate the second layer.
var buildTemplate = function ()
{
  var doctypeInfo = documents.info();
  var metaTemp = '{{=<% %>=}}\n' + templates['worksheet'].render(doctypeInfo);
  shimi.globals[worksheetName()] = Hogan.compile(metaTemp);

  return true;
};

// Render the worksheet.
var fillWorksheet = function ()
{
  var setName = worksheetsSet().val();
  var url = 'worksheets';
  var complete = function (_ignore, req)
  {
    var data = JSON.parse(req.responseText);
    var ws = shimi.globals[worksheetName()].render(data);
    worksheetsArea().html(ws);
  };

  if (!setName.isBlank())
  {
    var thisSet = setsui.getSet(setName)[1];

    if (thisSet.lenght <= 250)
    {
      var setIds = thisSet.map(function (x)
      {
        return x[1];
      });

      form.send(url, setIds, 'POST', complete);
    }
    else
    {
      flash.error('Could not load worksheet', 'the current set size is limited to 250 items.');
    }
  }

  return true;
};

exports(selectAllRows);
exports(rowSelection);
exports(columnSelection);
exports(showHandles);
exports(hideHandles);
exports(showFieldset);
exports(hideFieldset);
exports(showField);
exports(hideField);
exports(buildTemplate);
exports(fillWorksheet);
