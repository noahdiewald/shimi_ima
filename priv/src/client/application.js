// # The Client Code Entry Point
//
// *Implicit depends:* DOM, JQuery, JQueryUI
//
// This is the entry point for the client side code. This is where
// basic initializations take place and helper functions are added to
// JavaScript Objects. The 'onload' code is here.

// ## Variable Definitions

// A place to temporarily store global objects. Sometimes this is more
// convenient than using other types of client side storage. It is used
// rarely and explicitly using this object.
var shimi = {};
shimi.globals = {};

require('./jquery-ui-input-state.js');

var clickDispatch = require('./click-dispatch.js').clickDispatch;
var dblclickDispatch = require('./dblclick-dispatch.js').dblclickDispatch;
var changes = require('./changes.js').changes;
var keystrokes = require('./keystrokes.js').keystrokes;
var form = require('./form.js');

// These are the basic sub-application entry points.
var documents = require('./documents/documents.js');
var fm = require('./file_manager/fm.js');
var ilistingui = require('./index_tool/ilistingui.js');
var projectui = require('./projects/projectui.js');
var config = require('./config/config.js');

// ## Extensions to String and Array Objects

// ### Functions added to String

// This is a poorly implement `isBlank` predicate.
String.prototype.isBlank = function ()
{
  'use strict';

  return ((/^\s*$/).test(this) && !(/\S/).test(this));
};

// Remove white space at the beginning and end of string.
String.prototype.trim = function ()
{
  'use strict';

  return this.replace(/^\s+/, '').replace(/\s+$/, '');
};

// ### Functions added to Array

// Remove white space on all strings in array.
Array.prototype.trimAll = function ()
{
  'use strict';

  return this.map(function (i)
  {
    return i.trim();
  }).filter(function (i)
  {
    return !i.match(/^$/);
  });
};

// ## On Load

// Using the JQuery function for running code after the page loads.
$(function ()
{
  'use strict';

  // All clicks handled centraly
  $('body').click(function (e)
  {
    clickDispatch(e);
  });

  // All double clicks handled centraly
  $('body').dblclick(function (e)
  {
    dblclickDispatch(e);
  });

  // Other event handling
  keystrokes();
  changes();

  // Hide notification boxes.
  // TODO: move to stylesheets
  $('.notification').hide();

  // Hide ajax loading indicator.
  // TODO: move to stylesheets
  $('#loading').hide();

  // Show and hide the AJAX loading indicator.
  $(document).ajaxStart(function ()
  {
    $('#loading').show();
  }).ajaxStop(function ()
  {
    $('#loading').hide();
  });

  // Initialize any data fields, which use JQueryUI.
  form.initDateFields();

  // ### Determine the sub-application.
  //
  // TODO: With the CommonJS module system there should be a better way
  // of sharing code between these sub-applications.

  // Detect if this is the configuration sub-application
  if ($('#configuration').length > 0)
  {
    config.init();
  }

  // Detect if this is the document editing sub-application
  if ($('#all-document-container').length > 0)
  {
    documents.init();
  }

  // Detect if this is the file manager sub-application
  if ($('#file-upload').length > 0)
  {
    fm.init();
  }

  // Detect if this is the index tool sub-application
  if ($('#all-index-container').length > 0)
  {
    ilistingui.init();
  }

  // Detect if this is the project creation sub-application
  if ($('#projects-container').length > 0)
  {
    projectui.init();
  }
});
