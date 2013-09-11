// # The file manager
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Interface for working with CouchDB attachments within documents that
// exist only for the pupose of holding the attachment. A mock-file
// system path is given to these saved documents and may be used to
// retrieve them instead of the ID.

// Variable Definitions

var form = require('../form.js');
var flash = require('../flash.js');

// Internal functions

// Get information subdirectories within a path. As an example
// '/home/chuck/'.
var getDirListing = function (path)
{
  'use strict';

  if (path === undefined)
  {
    path = '';
  }

  $.get('file_manager/list_dirs/' + path, function (data)
  {
    $('#file-paths').html(data);
  });
};

// Get the document information for documents with a certain path.
var getFileListing = function (path)
{
  'use strict';

  if (path === undefined)
  {
    path = '';
  }

  $.get('file_manager/list_files/' + path, function (data)
  {
    $('#file-listing').html(data);
  });
};

// Open a dialog for editing a file path.
var pathEditDialog = function (obj, path)
{
  'use strict';

  var pathInput = $('#file-path-input');

  if (obj.path)
  {
    pathInput.val(obj.path.join('/'));
  }
  else
  {
    pathInput.val('');
  }

  var dialog = $('#edit-path-dialog').dialog(
  {
    autoOpen: false,
    modal: true,
    buttons:
    {
      'Move': function ()
      {
        var url = 'file_manager/' + obj._id + '?rev=' + obj._rev;
        var complete = function ()
        {
          refreshListings(path);
          flash.highlight('Success', 'File Moved');
        };

        obj.path = pathInput.val().replace(/^\s*|\s*$/g, '').replace(/\/+/g, '/').replace(/^\/|\/$/g, '').split('/');
        form.send(url, obj, 'PUT', complete, dialog);
        $(this).dialog('close');
      },
      'Cancel': function ()
      {
        $(this).dialog('close');
      }
    }
  });

  return dialog;
};

// Exported functions

// Initialize the sub-application.
var init = function ()
{
  'use strict';

  refreshListings();
  $('#file-upload-target').load(function ()
  {
    var encoded = $('#file-upload-target').contents().find('body pre').html();
    var obj = function ()
    {
      if (encoded && encoded.length > 0)
      {
        return JSON.parse(encoded);
      }
      else
      {
        return {
          message: false
        };
      }
    };

    if (obj() && obj().message && obj().status !== 'success')
    {
      flash.error('Error', obj().message);
      refreshListings();
    }
    else if (obj().message)
    {
      flash.highlight('Success', obj().message);
      refreshListings();
    }
  });
};

// Handle the mouse click action that initiates going to a directory.
var goDir = function (target)
{
  'use strict';

  var newpath = $(target).attr('data-path');
  window.sessionStorage.fmPath = newpath;
  refreshListings(newpath);

  return true;
};

// Return to the root directory.
var rootDir = function ()
{
  'use strict';

  var path = window.sessionStorage.fmPath = '';
  refreshListings();

  return true;
};

// Move up a directory.
var upDir = function ()
{
  'use strict';

  var path = window.sessionStorage.fmPath;
  var newpath = path.split('/');
  newpath.pop();
  newpath = newpath.join('/');
  window.sessionStorage.fmPath = newpath;

  refreshListings(newpath);

  return true;
};

// Handle the mouse click action that initiates editing a file by opening
// a dialog to edit its path.
var editFile = function (target)
{
  'use strict';

  var path = window.sessionStorage.fmPath;
  var fileId = target.attr('data-file-id');
  var url = 'file_manager/' + fileId;

  $.getJSON(url, function (obj)
  {
    pathEditDialog(obj, path).dialog('open');
  });

  return true;
};

// Handle the mouse click action that initiates deleting a file.
var deleteFile = function (target)
{
  'use strict';

  var path = window.sessionStorage.fmPath;
  var fileId = target.attr('data-file-id');
  var fileRev = target.attr('data-file-rev');
  var url = 'file_manager/' + fileId + '?rev=' + fileRev;
  var complete = function ()
  {
    refreshListings(path);
    flash.highlight('Success', 'File Deleted');
  };

  form.send(url, null, 'DELETE', complete, target);

  return true;
};

// Refresh the file listing using the given path.
var refreshListings = function (path)
{
  'use strict';
  getDirListing(path);
  getFileListing(path);
};

exports(init);
exports(goDir);
exports(rootDir);
exports(upDir);
exports(editFile);
exports(deleteFile);
exports(refreshListings);
