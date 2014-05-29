// # UI Shared
//
// *Implicit depends:* DOM
//
// UI elements and helper functions.

// ## Exported functions

// User interface element
var allDocContainer = function () {
  'use strict';

  return document.getElementById('all-document-container');
};

// User interface element
var container = function () {
  'use strict';

  return document.getElementById('container');
};

// User interface element
var createButton = function () {
  'use strict';

  return document.getElementById('create-document-button');
};

// User interface element
var deleteButton = function () {
  'use strict';

  return document.getElementById('document-delete-button');
};

// User interface element
var dv = function () {
  'use strict';

  return document.getElementById('document-view');
};

// User interface element
var dvt = function () {
  'use strict';

  return document.getElementById('document-view-tree');
};

// User interface element
var editButton = function () {
  'use strict';

  return document.getElementById('document-edit-button');
};

// User interface element
var editForm = function () {
  'use strict';

  return document.getElementById('edit-document-form');
};

// User interface element
var firstIndex = function () {
  'use strict';

  return document.getElementById('first-index-element');
};

// Hide an element
var hide = function (elem) {
  'use strict';

  elem.classList.add('hidden');

  return document;
};

// Hide the button.
var hideButton = function (button) {
  'use strict';

  hide(button);
  button.setAttribute('disabled', 'disabled');

  return true;
};

// User interface element
var indexIndexInput = function () {
  'use strict';

  return document.getElementById('index-index-input');
};

// User interface element
var restoreButton = function () {
  'use strict';

  return document.getElementById('document-restore-button');
};

// User interface element
var saveButton = function () {
  'use strict';

  return document.getElementById('save-document-button');
};

// Display the element.
var show = function (elem) {
  'use strict';

  elem.classList.remove('hidden');

  return document;
};

// Display the button.
var showButton = function (button) {
  'use strict';

  show(button);
  button.removeAttribute('disabled');

  return true;
};

var sid = function () {
  'use strict';

  var fi;

  return fi ? fi.dataset.firstId : undefined;
};

var skey = function () {
  'use strict';

  var fi;

  return fi ? fi.dataset.firstKey : undefined;
};

// User interface element
var viewInfo = function () {
  'use strict';

  return document.getElementById('document-view-info');
};

exports.allDocContainer = allDocContainer;
exports.container = container;
exports.createButton = createButton;
exports.deleteButton = deleteButton;
exports.dv = dv;
exports.dvt = dvt;
exports.editButton = editButton;
exports.editForm = editForm;
exports.firstIndex = firstIndex;
exports.hide = hide;
exports.hideButton = hideButton;
exports.indexIndexInput = indexIndexInput;
exports.restoreButton = restoreButton;
exports.saveButton = saveButton;
exports.show = show;
exports.showButton = showButton;
exports.sid = sid;
exports.skey = skey;
exports.viewInfo = viewInfo;