// # Documents sub-application
//
// *Implicit depends:* DOM, JQuery
//
// Shared document editing stuff plus initialization.

// Variable Definitions

var setsui = require('./setsui.js');
var editui = require('./editui.js');
var viewui = require('./viewui.js');
var indexui = require('./indexui.js');
var changeui = require('./changeui.js');
var sender = require('../sender.js').sender;
var store = require('../store.js').store;
var identifier;

// Internal functions

// In practice this is the select listing of the user created indexes
// which is triggering the change event.
//
// *TODO* put this with other change handlers.
var indexForm = function ()
{
  'use strict';

  $('#index-filter-form select').change(function ()
  {
    indexui.get();
  });

  return true;
};

// If there is a hash at the end of the URL with a document ID specified,
// this will pass the information on the correct funciont in `viewui`.
var loadHash = function (urlHash)
{
  'use strict';

  if (urlHash)
  {
    viewui.get(urlHash);
  }

  return true;
};

var allDocContainer = function ()
{
  'use strict';

  return $('#all-document-container');
};

// Key used in retrieving cached information from session storage.
var versionKey = function ()
{
  'use strict';

  return identifier() + '_version';
};

// Key used in retrieving cached information from session storage.
var infoKey = function ()
{
  'use strict';

  return identifier() + '_info';
};

// Key used in retrieving cached information from session storage.
var labelsKey = function ()
{
  'use strict';

  return identifier() + '_labels';
};

// Store the doctype info in the session store.
var storeDoctype = function (doctype)
{
  'use strict';

  sessionStorage.setItem(infoKey(), doctype);
  sender('doctype-info-ready');

  return true;
};

// Exported functions

// Get the stored doctype version.
var getVersion = function ()
{
  'use strict';

  return sessionStorage.getItem(versionKey());
};

// Get the most recent doctype version, which is placed in a `data`
// attribute that is updated on page reloads.
var getCurrentVersion = function ()
{
  'use strict';

  return store(allDocContainer()).d('version');
};

// Check if the stored doctype version matches the version found in the
// `data` attribute.
var isCurrentVersionStored = function ()
{
  'use strict';

  return (getVersion() && getVersion() === getCurrentVersion());
};

// Reset the doctype version
var setVersion = function ()
{
  'use strict';

  sessionStorage.setItem(versionKey(), getCurrentVersion());
  sender('version-set');

  return true;
};

// Clear the session storage
var clearSession = function ()
{
  'use strict';

  sessionStorage.clear();
  sender('session-cleared');

  return true;
};

// Check if the doctype version stored is current and report the current
// state based on the result.
var checkVersion = function ()
{
  'use strict';

  if (isCurrentVersionStored())
  {
    sender('labels-ready');
  }
  else
  {
    sender('bad-session-state');
  }

  return true;
};

// Get the doctype name
var dname = function ()
{
  'use strict';

  return store($('#all-document-container')).d('doctype');
};

// Get the project id
var project = function ()
{
  'use strict';

  return store($('#container')).get('project-id');
};

// Identifier is a combination of the project and doctype name.
identifier = function ()
{
  'use strict';

  return project() + '_' + dname();
};

// Get information about doctype.
var info = function ()
{
  'use strict';

  return JSON.parse(sessionStorage.getItem(infoKey()));
};

// Load the doctype document stored on the server.
var loadDoctype = function ()
{
  'use strict';

  $.getJSON('./', function (data)
  {
    storeDoctype(JSON.stringify(data));
  });

  return true;
};

// Process the field and fieldset info to create a field label to field
// id index.
var makeLabels = function ()
{
  'use strict';

  var info = info();
  var labels = {};

  info.fieldsets.forEach(function (fieldset)
  {
    fieldset.fields.forEach(function (field)
    {
      labels[field._id] = [fieldset.label, field.label];
    });
  });

  sessionStorage.setItem(labelsKey(), JSON.stringify(labels));
  sender('labels-ready');

  return true;
};

// Initialize the documents sub-application.
var init = function ()
{
  'use strict';

  $('form').on('submit', function ()
  {
    return false;
  });
  checkVersion();
  setsui.updateSelection();
  indexui.iOpts();
  indexui.get();
  indexForm();
  editui.init();
  loadHash($(location)[0].hash.split('#')[1]);
  changeui.get();
};

exports(getVersion);
exports(getCurrentVersion);
exports(isCurrentVersionStored);
exports(setVersion);
exports(clearSession);
exports(checkVersion);
exports(dname);
exports(project);
exports(identifier);
exports(info);
exports(loadDoctype);
exports(makeLabels);
exports(init);
