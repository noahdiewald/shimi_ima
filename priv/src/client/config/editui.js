// # Config Editor
//
// *Implicit depends:* DOM
//
// All code for working with the editor.

// ## Variable Definitions

var formalize = require('../formalize.js');
var ajax = require('../ajax.js');
var S = require('../sender.js');

// ## Internal Functions

// Toggle the visibility of a group.
var toggle = function (node) {
  'use strict';

  node.classList.toggle('collapsed');
  node.nextSibling.classList.toggle('hidden');

  return 'toggled-subgroup';
};

// Get the editor form object.
var editForm = function () {
  'use strict';

  return document.getElementById('edit-form');
};

var fillForm = function (json) {
  'use strict';

  var formHTML = formalize.toForm(json);
  var form = editForm();

  form.innerHTML = formHTML;

  return 'form-filled';
};

var determineCategory = function (json) {
  'use strict';

  var obj = JSON.parse(json);

  return obj.category;
};

// ## Exported Functions

// Get the specified stored document and load it into the editor.
var get = function (url) {
  'use strict';

  var complete = function (req) {
    return fillForm(JSON.stringify(req.response));
  };

  ajax.get(url, complete);

  return 'object-loaded';
};

// Load an empty object into the editor.
var fresh = function () {
  'use strict';

  fillForm('{}');

  return 'empty-object-loaded';
};

var create = function () {
  'use strict';

  var form = editForm();
  var json = formalize.fromForm(form.innerHTML);
  var category = determineCategory(json);
  var complete = function () {
    S.sender('config-' + category + '-created');
  };

  ajax.post('config/' + category + 's', json, complete);

  return 'object-created';
};

var update = function (args) {
  'use strict';

  return 'object-updated';
};

var remove = function (args) {
  'use strict';

  return 'object-removed';
};

var restore = function (args) {
  'use strict';

  return 'object-restored';
};

// Initialize the editor, loading a fresh object.
var init = function (json) {
  'use strict';

  if (json) {
    fillForm(json);
  } else {
    fresh();
  }

  return 'editor-initialized';
};

exports.init = init;
exports.get = get;
exports.fresh = fresh;
exports.update = update;
exports.create = create;
exports.remove = remove;
exports.restore = restore;
exports.toggle = toggle;
