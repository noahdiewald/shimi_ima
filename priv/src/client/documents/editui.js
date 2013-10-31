// # Documents sub-application
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Edit pane UI elements

// Variable Definitions

var store = require('../store.js').store;
var form = require('../form.js');
var flash = require('../flash.js');
var ajax = require('../ajax.js');
var fieldsets = require('./fieldsets.js');
var viewui = require('./viewui.js');
var indexui = require('./indexui.js');
var afterRefresh;

// Internal functions

// UI Element
var saveButton = function ()
{
  'use strict';

  return $('#save-document-button');
};

// UI Element
var createButton = function ()
{
  'use strict';

  return $('#create-document-button');
};

// UI Element
var editButton = function ()
{
  'use strict';

  return $('#document-edit-button');
};


// Display validation error properly.
var validationError = function (req)
{
  'use strict';

  var body = JSON.parse(req.responseText);
  var title = req.statusText;

  var invalid = $('[data-field-instance=' + body.instance + ']');
  var invalidTab = $('[href=#' + invalid.parents('fieldset').attr('id') + ']').parent('li');

  invalidTab.addClass('ui-state-error');
  invalid.addClass('ui-state-error');

  flash.error(title, body.fieldname + ' ' + body.message);

  return true;
};

// Fields need to have instances. This should ensure they have them.
var instances = function (addInstances)
{
  'use strict';

  var text = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];
  var makeInstance = function ()
  {
    return text.map(function ()
    {
      return text[Math.floor(Math.random() * text.length)];
    }).join('');
  };

  $('#last-added [data-field-instance]').each(function (index, item)
  {
    var itemElem = $(item).first();
    var oldInstance = itemElem.attr('data-field-instance');
    var newInstance = oldInstance;

    if (addInstances)
    {
      newInstance = makeInstance();
    }

    itemElem.attr('data-group-id', newInstance);
    // TODO: This is a little redundant
    itemElem.attr('id', newInstance);
    itemElem.attr('data-field-instance', newInstance);
    // Differences in Firefox and Chrome
    itemElem.next('.expander').attr('data-group-id', newInstance);
    itemElem.next().next('.expander').attr('data-group-id', newInstance);
  });

  if (addInstances)
  {
    $('#last-added').removeAttr('id');
  }

  return true;
};

// Exported functions

// Initialize the editing pane.
var init = function ()
{
  'use strict';

  var url = 'documents/edit';

  ajax.legacyHTMLGet(url, function (req)
  {
    var documentEditHtml = req.response;

    $('#document-edit').html(documentEditHtml);
    $('#edit-tabs').tabs();
    fieldsets.initFieldsets();
  });

  return true;
};

// Focus on the first focusable input element in an active tab.
var selectInput = function ()
{
  'use strict';

  var inputable = 'input, select, textarea';
  var t = function ()
  {
    return $('#edit-tabs');
  };

  var cur = t().find('.ui-tabs-active a').attr('href');
  $(cur).find(inputable).first().focus();

  return true;
};

// Used as a variation of `afterRefresh` where a boolean is provided
// to specify if new instances identifiers should be created and
// set. Basically this is for a completely fresh refresh, when the form
// is in the state such that a document can be created but no information
// is available to do an update.
var afterFreshRefresh = function (addInstances)
{
  'use strict';

  afterRefresh(addInstances);

  return true;
};

// Run after the edit button in the view UI is clicked.
var afterEditRefresh = function ()
{
  'use strict';

  var sharedAttrs = ['data-document-id', 'data-document-rev'];

  sharedAttrs.forEach(function (elem)
  {
    saveButton().attr(elem, editButton().attr(elem));
  });

  saveButton().show();
  afterRefresh();

  return true;
};

// Essentially initialization of the form. If `addInstances` is true,
// new instance identifiers will be created for a blank form.
afterRefresh = function (addInstances)
{
  'use strict';

  form.initDateFields();
  instances(addInstances);

  return true;
};

// Reset field values to defaults.
var resetFields = function ()
{
  'use strict';

  $('.field').each(function (index)
  {
    var field = $(this);
    var thedefault = field.attr('data-field-default');

    if (thedefault && thedefault !== '')
    {
      if (field.is('select.multiselect'))
      {
        field.val(thedefault.split(','));
      }
      else if (field.is('input.boolean'))
      {
        field.attr('checked', thedefault === true);
      }
      else
      {
        field.val(thedefault);
      }
    }
    else
    {
      field.val('');
      field.removeAttr('checked');
    }
  });

  return true;
};

// To be run if the user chooses to save the form contents. This is an
// update, not creation.
var save = function ()
{
  'use strict';

  if (saveButton().hasClass('oldrev'))
  {
    if (!window.confirm('This data is from an older version of this document. Are you sure you want to restore it?'))
    {
      return false;
    }
  }

  var body;
  var title;
  var s = store(saveButton());
  var root = $('#edit-document-form');
  var document = s.d('document');
  var rev = s.d('rev');
  var url = './documents/' + document + '?rev=' + rev;
  var skey = $('#first-index-element').attr('data-first-key');
  var sid = $('#first-index-element').attr('data-first-id');
  var obj = {
    doctype: s.d('doctype'),
    description: s.d('description')
  };

  $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
  saveButton().hide();
  $.extend(obj, fieldsets.fieldsetsToObject(root));

  $.ajax(
  {
    type: 'PUT',
    url: url,
    dataType: 'json',
    contentType: 'application/json',
    processData: false,
    data: JSON.stringify(obj),
    complete: function (req, status)
    {
      if (req.status === 204 || req.status === 200)
      {
        title = 'Success';
        body = 'Your document was saved.';
        viewui.get(document);
        indexui.get(skey, sid);
        flash.highlight(title, body);
        saveButton().removeClass('oldrev').show();
      }
      else if (req.status === 403)
      {
        validationError(req);
        saveButton().show();
      }
      else if (req.status === 409)
      {
        body = JSON.parse(req.responseText);
        title = req.statusText;

        flash.error(title, body.message);
        saveButton().hide();
      }
    }
  });
};

// To be run if creating a new document.
var create = function ()
{
  'use strict';

  var s = store(createButton());
  var root = $('#edit-document-form');
  var skey = $('#first-index-element').attr('data-first-key');
  var sid = $('#first-index-element').attr('data-first-id');
  var obj = {
    doctype: s.d('doctype'),
    description: s.d('description')
  };

  $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
  createButton().hide();
  $.extend(obj, fieldsets.fieldsetsToObject(root));

  var postUrl = $.ajax(
  {
    type: 'POST',
    dataType: 'json',
    contentType: 'application/json',
    processData: false,
    data: JSON.stringify(obj),
    complete: function (req, status)
    {
      if (req.status === 201)
      {
        var title = 'Success';
        var body = 'Your document was created.';
        var documentId = postUrl.getResponseHeader('Location').match(/[a-z0-9]*$/);

        saveButton().hide().attr('disabled', 'true');
        $('.fields').remove();
        fieldsets.initFieldsets();
        viewui.get(documentId);
        indexui.get(skey, sid);
        flash.highlight(title, body);
        createButton().show();
      }
      else if (req.status === 403)
      {
        validationError(req);
        createButton().show();
      }
    }
  });
};

// Clear the form.
var clear = function ()
{
  'use strict';

  $('#edit-document-form .ui-state-error').removeClass('ui-state-error');
  saveButton().hide().attr('disabled', 'disabled');
  $('.fields').remove();
  fieldsets.initFieldsets();
};

// Display a help dialog for a form field.
var showHelpDialog = function (target)
{
  'use strict';

  if (target.is('.label-text'))
  {
    target = target.parent('label').find('.ui-icon-help');
  }

  $('#help-dialog').dialog().dialog('open').find('#help-dialog-text').html(target.attr('title'));

  return true;
};

// Contract and expand textarea elements.
var toggleTextarea = function (target)
{
  'use strict';

  var textarea = $('#' + target.attr('data-group-id'));

  if (target.attr('id') === textarea.attr('data-group-id'))
  {
    textarea.toggleClass('expanded');
    textarea.next().next('span').toggleClass('expanded');
  }
  else
  {
    textarea.toggleClass('expanded');
    target.toggleClass('expanded');
  }

  return true;
};

exports.init = init;
exports.selectInput = selectInput;
exports.afterFreshRefresh = afterFreshRefresh;
exports.afterEditRefresh = afterEditRefresh;
exports.afterRefresh = afterRefresh;
exports.resetFields = resetFields;
exports.save = save;
exports.create = create;
exports.clear = clear;
exports.toggleTextarea = toggleTextarea;
