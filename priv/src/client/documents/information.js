// # Document Information
//
// *Implicit depends:* DOM
//
// Store and retrieve global information about doctypes and context.

// ## Imported Modules

var S = require('../sender.js');
var ajax = require('ajax');
var ui = require('documents/ui-shared');

// ## Exported Function Names

var checkState;
var clearSession;
var doctypeId;
var identifier;
var info;
var loadDoctype;
var makeFieldsetLookup;
var makeLabels;
var project;
var setsKey;
var setVersion;
var worksheetName;

// ## Internal Function Names

var fieldsToFieldsetKey;
var getCurrentVersion;
var getVersion;
var infoKey;
var isAllDataStored;
var isCurrentVersionStored;
var isFieldsToFieldsetStored;
var isInfoStored;
var isLabelsStored;
var labelsKey;
var storeDoctype;
var versionKey;

// ## Internal Functions

// Key used in retrieving cached information from session storage.
fieldsToFieldsetKey = function () {
  'use strict';

  return identifier() + '_fieldsToFieldset';
};

// Get the most recent doctype version, which is placed in a `data`
// attribute that is updated on page reloads.
getCurrentVersion = function () {
  'use strict';

  return ui.allDocContainer().dataset.documentVersion;
};

// Get the stored doctype version.
getVersion = function () {
  'use strict';

  return sessionStorage.getItem(versionKey());
};

// Key used in retrieving cached information from session storage.
infoKey = function () {
  'use strict';

  return identifier() + '_info';
};

// True if the data that should be stored is stored.
isAllDataStored = function () {
  'use strict';

  return isCurrentVersionStored() && isInfoStored() && isLabelsStored() && isFieldsToFieldsetStored();
};

// Check if the stored doctype version matches the version found in the
// `data` attribute.
isCurrentVersionStored = function () {
  'use strict';

  return (getVersion() && getVersion() === getCurrentVersion());
};

// Is the field to fieldsets index stored?
isFieldsToFieldsetStored = function () {
  'use strict';

  return sessionStorage.getItem(fieldsToFieldsetKey()) !== null;
};

// Is the doctype information stored?
isInfoStored = function () {
  'use strict';

  return sessionStorage.getItem(infoKey()) !== null;
};

// Is the field plus fieldset to labels index stored?
isLabelsStored = function () {
  'use strict';

  return sessionStorage.getItem(labelsKey()) !== null;
};

// Key used in retrieving cached information from session storage.
labelsKey = function () {
  'use strict';

  return identifier() + '_labels';
};

// Store the doctype info in the session store.
storeDoctype = function (doctype) {
  'use strict';

  sessionStorage.setItem(infoKey(), doctype);

  return S.sender('doctype-info-ready');
};

// Key used in retrieving cached information from session storage.
versionKey = function () {
  'use strict';

  return identifier() + '_version';
};

// ## Exported Functions

// Check the session state to ensure it is up to date and fully
// loaded.
checkState = function () {
  'use strict';

  var retval;

  if (isAllDataStored()) {
    retval = S.sender('doctype-cached-info-ready');
  } else {
    retval = S.sender('bad-session-state');
  }

  return retval;
};

// Clear the session storage
clearSession = function () {
  'use strict';

  sessionStorage.clear();
  S.sender('session-cleared');

  return true;
};

// Get the doctype name
doctypeId = function () {
  'use strict';

  return ui.allDocContainer().dataset.documentDoctype;
};

// Identifier is a combination of the project and doctype name.
identifier = function () {
  'use strict';

  return project() + '_' + doctypeId();
};

// Get information about doctype.
info = function () {
  'use strict';

  var documentInfo = JSON.parse(sessionStorage.getItem(infoKey()));

  return documentInfo;
};

// Load the doctype document stored on the server.
loadDoctype = function () {
  'use strict';

  ajax.get('./', function (req) {
    storeDoctype(JSON.stringify(req.response));
  });

  return true;
};

// Process the field and fieldset info to create a field id to fieldset
// id index.
makeFieldsetLookup = function () {
  'use strict';

  var lookup = {};

  info().fieldsets.forEach(function (fieldset) {
    fieldset.fields.forEach(function (field) {
      lookup[field._id] = fieldset._id;
    });
  });

  sessionStorage.setItem(fieldsToFieldsetKey(), JSON.stringify(lookup));

  return S.sender('fieldset-lookup-ready');
};

// Process the field and fieldset info to create a field id to field
// label index.
makeLabels = function () {
  'use strict';

  var labels = {};

  info().fieldsets.forEach(function (fieldset) {
    fieldset.fields.forEach(function (field) {
      labels[field._id] = [fieldset.label, field.label];
    });
  });

  sessionStorage.setItem(labelsKey(), JSON.stringify(labels));

  return S.sender('doctype-cached-info-ready');
};

// Get the project id
project = function () {
  'use strict';

  return ui.container().dataset.projectId;
};

// Get the key that stores sets.
setsKey = function () {
  'use strict';

  return identifier() + '_sets';
};

// Reset the doctype version
setVersion = function () {
  'use strict';

  sessionStorage.setItem(versionKey(), getCurrentVersion());
  S.sender('version-set');

  return true;
};

// Name for the worksheet template.
var worksheetName = function () {
  'use strict';

  return identifier() + '_worksheet-template';
};

exports.checkState = checkState;
exports.clearSession = clearSession;
exports.doctypeId = doctypeId;
exports.identifier = identifier;
exports.info = info;
exports.loadDoctype = loadDoctype;
exports.makeFieldsetLookup = makeFieldsetLookup;
exports.makeLabels = makeLabels;
exports.project = project;
exports.setsKey = setsKey;
exports.setVersion = setVersion;
exports.worksheetName = worksheetName;
