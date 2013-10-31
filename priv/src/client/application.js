// # The Client Code Entry Point
//
// *Implicit depends:* DOM, JQuery, JQueryUI
//
// This is the entry point for the client side code. This is where
// basic initializations take place and helper functions are added to
// JavaScript Objects. The 'onload' code is here.

// ## Variable Definitions

var exports = module.exports;

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

// Camel case string
String.prototype.cc = function ()
{
  'use strict';

  return this.replace(/-./, function (substr)
  {
    return substr.toUpperCase()[1];
  });
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

// ### Functions added to Element

if (Element.prototype.mozMatchesSelector)
{
  Element.prototype.matches = Element.prototype.mozMatchesSelector;
}
else if (Element.prototype.webkitMatchesSelector)
{
  Element.prototype.matches = Element.prototype.webkitMatchesSelector;
}
else
{
  throw 'This browser is not supported at this time. An implementation of Element.matches is needed https://developer.mozilla.org/en-US/docs/Web/API/Element.matches';
}

// ## Initialization

// Using the function for running code after the page loads.
var init = function ()
{
  'use strict';

  // All clicks handled centraly
  document.body.onclick = clickDispatch;

  // All double clicks handled centraly
  document.body.ondblclick = dblclickDispatch;

  // Other event handling
  keystrokes();
  changes();

  // Show and hide the AJAX loading indicator.
  $(document).ajaxStart(function ()
  {
    document.getElementById('loading').style.display = 'block';
  }).ajaxStop(function ()
  {
    document.getElementById('loading').style.display = 'none';
  });

  // Initialize any data fields, which use JQueryUI.
  form.initDateFields();

  // ### Determine the sub-application.
  //
  // TODO: With the CommonJS module system there should be a better way
  // of sharing code between these sub-applications.

  // Detect if this is the configuration sub-application
  if ($('#all-config-container').length > 0)
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
};

document.onreadystatechange = function ()
{
  'use strict';

  if (document.readyState === 'complete')
  {
    init();
  }
};
