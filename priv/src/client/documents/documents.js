// # Documents sub-application
//
// *Implicit depends:* DOM, JQuery
//
// Shared document editing stuff plus initialization.

// ## Variable Definitions

var setsui = require('./setsui.js');
var editui = require('./editui.js');
var viewui = require('./viewui.js');
var indexui = require('./indexui.js');
var changeui = require('./changeui.js');
var S = require('../sender.js');
var store = require('../store.js').store;
var ajax = require('../ajax.js');
var identifier;

// ## Internal functions

// In practice this is the select listing of the user created indexes
// which is triggering the change event.
//
// *TODO* put this with other change handlers.
var indexForm = function ()
{
  'use strict';

  // TODO Remove JQuery
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

// A user interface element.
var allDocContainer = function ()
{
  'use strict';

  return document.getElementById('all-document-container');
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

  return S.sender('doctype-info-ready');
};

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

var isInfoStored = function ()
{
  'use strict';

  return sessionStorage.getItem(infoKey()) !== null;
};

var isLabelsStored = function ()
{
  'use strict';

  return sessionStorage.getItem(labelsKey()) !== null;
};

// Reset the doctype version
var setVersion = function ()
{
  'use strict';

  sessionStorage.setItem(versionKey(), getCurrentVersion());
  S.sender('version-set');

  return true;
};

// Check the session state to ensure it is up to date and fully
// loaded.
var checkState = function ()
{
  'use strict';

  var retval;

  if (isCurrentVersionStored() && isInfoStored() && isLabelsStored())
  {
    retval = S.sender('labels-ready');
  }
  else
  {
    retval = S.sender('bad-session-state');
  }

  return retval;
};

// Get the doctype name
var dname = function ()
{
  'use strict';

  return store(allDocContainer()).d('doctype');
};

// Get the project id
var project = function ()
{
  'use strict';

  var container = document.getElementById('container');
  return store(container).get('project-id');
};

// ## Exported functions

// Clear the session storage
var clearSession = function ()
{
  'use strict';

  sessionStorage.clear();
  S.sender('session-cleared');

  return true;
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

  ajax.get('./', function (req)
  {
    storeDoctype(JSON.stringify(req.response));
  });

  return true;
};

// Process the field and fieldset info to create a field label to field
// id index.
var makeLabels = function ()
{
  'use strict';

  var info1 = info();
  var labels = {};

  info1.fieldsets.forEach(function (fieldset)
  {
    fieldset.fields.forEach(function (field)
    {
      labels[field._id] = [fieldset.label, field.label];
    });
  });

  sessionStorage.setItem(labelsKey(), JSON.stringify(labels));

  return S.sender('labels-ready');
};

// Initialize the documents sub-application.
var init = function ()
{
  'use strict';

  // TODO Remove JQuery
  $('form').on('submit', function ()
  {
    return false;
  });
  checkState();
  setsui.updateSelection();
  indexui.iOpts();
  indexui.get();
  indexForm();
  editui.init();
  // TODO remove JQuery
  loadHash($(location)[0].hash.split('#')[1]);
  changeui.get();
};

exports.setVersion = setVersion;
exports.clearSession = clearSession;
exports.identifier = identifier;
exports.info = info;
exports.loadDoctype = loadDoctype;
exports.makeLabels = makeLabels;
exports.init = init;
