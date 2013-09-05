// # The view user interface
//
// *Implicit depends:* DOM, JQuery, Hogan, templates
//
// View pane UI elements.
//
// *TODO* I may be exporting more than needed.

// Variable Definitions

var store = require('../store.js').store;
var indexui = require('./indexui.js');
var flash = require('../flash.js');
var editui = require('./editui.js');
var fieldsets = require('./fieldsets.js');

// Internal functions

// User interface element
var dv = function ()
{
  return $('#document-view');
};

// User interface element
var dvt = function ()
{
  return $('#document-view-tree');
};

// User interface element
var viewInfo = function ()
{
  return $('#document-view-info');
};

// Make an object where fieldsets with deletions are identified.
var getDeletions = function (changes)
{
  return Object.keys(changes).reduce(function (acc, x)
  {
    // If it was changed and there is no new value, it was deleted.
    if (changes[x].newValue === undefined)
    {
      if (acc[changes[x].fieldset] === undefined)
      {
        acc[changes[x].fieldset] = {};
      }
      acc[changes[x].fieldset][x] = changes[x];
    }

    return acc;
  },
  {});
};

// Process the document from the server.
var processIncoming = function (docJson, rev)
{
  var withDeletions = {};

  if (docJson.changes)
  {
    withDeletions = getDeletions(docJson.changes);
  }

  docJson.fieldsets.forEach(function (fset)
  {
    var fsetId = fset.id;

    if (withDeletions[fsetId] !== undefined)
    {
      fset.removal = true;
      fset.altered = true;
    }

    var fieldFunc = function (field)
    {
      var changes = {};
      var change;

      if (docJson.changes)
      {
        changes = docJson.changes;
      }
      change = changes[field.instance];

      field.json_value = JSON.stringify(field.value);

      if (change !== undefined)
      {
        field.changed = true;
        fset.altered = true;

        if (change.originalValue === undefined)
        {
          fset.addition = true;
          field.newfield = true;
        }
        else
        {
          field.originalValue = JSON.parse(change.originalValue);
        }
      }

      if (field.subcategory === 'textarea')
      {
        field.is_textarea = true;
      }
      else if (field.value && field.subcategory.match('multi'))
      {
        field.value = field.value.join(', ');
      }

      return true;
    };

    if (fset.multiple)
    {
      fset.multifields.forEach(function (mfs)
      {
        mfs.fields.forEach(function (field)
        {
          fieldFunc(field);
          return true;
        });
      });
    }
    else
    {
      fset.fields.forEach(function (field)
      {
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
var formatTimestamps = function ()
{
  $('.timestamp').each(

  function (i, item)
  {
    var newDate = (new Date($(item).text())).toLocaleString();
    if (newDate !== 'Invalid Date')
    {
      $(item).text(newDate);
    }
  });

  return true;
};

// Get the document.
var get = function (id, rev, callback)
{
  var url = 'documents/' + id;
  var htmlTarget = dv();
  var tmpl;

  if (rev)
  {
    url = url + '/' + rev;
    htmlTarget = dvt();
    tmpl = function (docJson)
    {
      return templates['document-view-tree'].render(docJson,
      {
        'document-view-field': templates['document-view-field']
      });
    };
  }
  else
  {
    tmpl = function (docJson)
    {
      return templates['document-view'].render(docJson,
      {
        'document-view-tree': templates['document-view-tree'],
        'document-view-field': templates['document-view-field']
      });
    };

  }

  $.getJSON(url, function (docJson)
  {
    var documentHtml;

    processIncoming(docJson, rev);
    documentHtml = tmpl(docJson);
    htmlTarget.html(documentHtml);
    window.location.hash = id;
    formatTimestamps();
    dv().fadeTo('slow', 1);
    if (callback)
    {
      callback();
    }

    if (rev)
    {
      $('#document-view-tree').addClass('oldrev');
    }
    else
    {
      var restoreButton = $('#document-restore-button');
      var editButton = $('#document-edit-button');
      var deleteButton = $('#document-delete-button');

      if (store(restoreButton).d('deleted') === 'true')
      {
        editButton.hide();
        deleteButton.hide();
        restoreButton.show();
      }
    }
  });

  return true;
};

// Restore the state of a document to that of an earlier revision.
var restore = function (id, rev)
{
  var url = './documents/' + id + '?rev=' + rev;
  var restoreButton = $('#document-restore-button');
  var skey = $('#first-index-element').attr('data-first-key');
  var sid = $('#first-index-element').attr('data-first-id');
  var body;
  var title;

  $.ajax(
  {
    type: 'DELETE',
    url: url,
    dataType: 'json',
    contentType: 'application/json',
    complete: function (req, status)
    {
      if (req.status === 200)
      {
        title = 'Success';
        body = 'Your document was restored.';

        get(id, null, function ()
        {
          dv().fadeTo('slow', 1);
          indexui.get(skey, sid);
        });
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
        body = 'Document was erased and cannot be restored.';
        title = req.statusText;

        flash.error(title, body);
      }
    }
  });

  return true;
};

// Delete the document.
var del = function (id, rev)
{
  var url = './documents/' + id + '?rev=' + rev;
  var restoreButton = $('#document-restore-button');
  var skey = $('#first-index-element').attr('data-first-key');
  var sid = $('#first-index-element').attr('data-first-id');
  var body;
  var title;

  $.ajax(
  {
    type: 'DELETE',
    url: url,
    dataType: 'json',
    contentType: 'application/json',
    complete: function (req, status)
    {
      if (req.status === 200)
      {
        title = 'Success';
        body = 'Your document was deleted.';
        var response = JSON.parse(req.responseText);

        store(restoreButton).put('document-rev', response.rev);

        $('#document-delete-button').hide();
        $('#document-edit-button').hide();
        restoreButton.show();
        dv().fadeTo('slow', 0.5);

        indexui.get(skey, sid);
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
        body = 'Document appears to have been deleted already.';
        title = req.statusText;

        flash.error(title, body);
      }
    }
  });

  return true;
};

// Confirm an action.
var confirmIt = function (callback)
{
  if (window.confirm('Are you sure?'))
  {
    var s = store(viewInfo());
    var id = s.d('document');
    var rev = s.d('rev');

    callback(id, rev);
  }

  return true;
};

// Move the document to the editor.
var edit = function ()
{
  editui.resetFields();
  if ($('#document-view-tree').hasClass('oldrev'))
  {
    $('#save-document-button').addClass('oldrev');
  }
  else
  {
    $('#save-document-button').removeClass('oldrev');
  }
  fieldsets.fillFieldsets();

  return true;
};

// Ask for confirmation on deletion.
var confirmDelete = function ()
{
  var s = store(viewInfo());
  var id = s.d('document');
  var rev = s.d('rev');
  return confirmIt(function ()
  {
    del(id, rev);
  });
};

// Ask for confirmation on restoration.
var confirmRestore = function ()
{
  var s = store(viewInfo());
  var id = s.d('document');
  var rev = s.d('rev');
  return confirmIt(function ()
  {
    restore(id, rev);
  });
};

// Expand and collapse elements of the view tree.
var collapseToggle = function (target)
{
  $(target).parent('li').toggleClass('collapsed');

  return true;
};

// Get a previous revision.
var fetchRevision = function (target)
{
  var s = store($(target));
  var id = s.d('document');
  var oldrev = s.d('oldrev');

  $('.revision-link').removeClass('selected-revision');
  $(target).addClass('selected-revision');

  get(id, oldrev);

  return true;
};

exports(formatTimestamps);
exports(get);
exports(restore);
exports(del);
exports(confirmIt);
exports(edit);
exports(confirmDelete);
exports(confirmRestore);
exports(collapseToggle);
exports(fetchRevision);
