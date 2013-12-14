// # The file manager
//
// *Implicit depends:* DOM, JQuery, JQuery UI
//
// Interface for working with CouchDB attachments within documents that
// exist only for the pupose of holding the attachment. A mock-file
// system path is given to these saved documents and may be used to
// retrieve them instead of the ID.

// Variable Definitions

var ajax = require('../ajax.js');
var flash = require('../flash.js');
var refreshListings;

// Internal functions

// Get information subdirectories within a path. As an example
// '/home/chuck/'.
var getDirListing = function (path) {
  'use strict';

  if (path === undefined) {
    path = '';
  }

  ajax.legacyHTMLGet('file_manager/list_dirs/' + path, function (req) {
    $('#file-paths').html(req.response);
  });
};

// Get the document information for documents with a certain path.
var getFileListing = function (path) {
  'use strict';

  if (path === undefined) {
    path = '';
  }

  ajax.legacyHTMLGet('file_manager/list_files/' + path, function (req) {
    $('#file-listing').html(req.response);
  });
};

// Open a dialog for editing a file path.
var pathEditDialog = function (obj, path) {
  'use strict';

  var pathInput = $('#file-path-input');

  if (obj.path) {
    pathInput.val(obj.path.join('/'));
  } else {
    pathInput.val('');
  }

  var dialog = $('#edit-path-dialog').dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      'Move': function () {
        var url = 'file_manager/' + obj._id + '?rev=' + obj._rev;
        var complete = function () {
          refreshListings(path);
          flash.highlight('Success', 'File Moved');
        };

        obj.path = pathInput.val().replace(/^\s*|\s*$/g, '').replace(/\/+/g, '/').replace(/^\/|\/$/g, '').split('/');
        ajax.put(url, obj, complete);
        $(this).dialog('close');
      },
      'Cancel': function () {
        $(this).dialog('close');
      }
    }
  });

  return dialog;
};

// Exported functions

// Initialize the sub-application.
var init = function () {
  'use strict';

  refreshListings();
  $('#file-upload-target').load(function () {
    var encoded = $('#file-upload-target').contents().find('body pre').html();
    var obj = function () {
      if (encoded && encoded.length > 0) {
        return JSON.parse(encoded);
      } else {
        return {
          message: false
        };
      }
    };

    if (obj() && obj().message && obj().status !== 'success') {
      flash.error('Error', obj().message);
      refreshListings();
    } else if (obj().message) {
      flash.highlight('Success', obj().message);
      refreshListings();
    }
  });
};

// Handle the mouse click action that initiates going to a directory.
var goDir = function (target) {
  'use strict';

  var newpath = $(target).attr('data-path');
  window.sessionStorage.fmPath = newpath;
  refreshListings(newpath);

  return true;
};

// Return to the root directory.
var rootDir = function () {
  'use strict';

  var path = window.sessionStorage.fmPath = '';
  refreshListings();

  return true;
};

// Move up a directory.
var upDir = function () {
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
var editFile = function (target) {
  'use strict';

  var path = window.sessionStorage.fmPath;
  var fileId = target.attr('data-file-id');
  var url = 'file_manager/' + fileId;

  ajax.get(url, function (req) {
    pathEditDialog(req.response, path).dialog('open');
  });

  return true;
};

// Handle the mouse click action that initiates deleting a file.
var deleteFile = function (target) {
  'use strict';

  var path = window.sessionStorage.fmPath;
  var fileId = target.attr('data-file-id');
  var fileRev = target.attr('data-file-rev');
  var url = 'file_manager/' + fileId + '?rev=' + fileRev;
  var complete = function () {
    refreshListings(path);
    flash.highlight('Success', 'File Deleted');
  };

  ajax.del(url, complete);

  return true;
};

// Refresh the file listing using the given path.
refreshListings = function (path) {
  'use strict';

  getDirListing(path);
  getFileListing(path);
};

exports.init = init;
exports.goDir = goDir;
exports.rootDir = rootDir;
exports.upDir = upDir;
exports.editFile = editFile;
exports.deleteFile = deleteFile;
exports.refreshListings = refreshListings;
