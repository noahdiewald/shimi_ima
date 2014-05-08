// # Doctype Listing
//
// *Implicit depends:* DOM
//
// Loads a listing of doctypes that can be edited.

var templates = require('templates');
var pager = require('pager').pager;
var S = require('../sender.js');
var uuid = require('node-uuid');

// ## Exported Functions

// Return the 'prefix' which is used in id and class names for
// elements used to page through these values.
var prefix = function () {
  'use strict';

  return 'doctypes';
};

// Called by a keystroke event handler when user changes form values.
var get = function () {
  'use strict';

  var url = 'config/' + prefix();
  var target = document.getElementById(prefix() + '-listing');

  pager({
    prefix: prefix(),
    url: url,
    target: target
  }).get();

  return 'doctype-listing-requested';
};

// Initialization
var init = function () {
  'use strict';

  get();

  return 'doctypeui-initialized';
};

// Begin the process of adding a doctype by sending JSON to the
// editor.
var addDoctype = function () {
  'use strict';

  var obj = {
    _id: uuid.v4().replace(/-/g, ''),
    category: 'doctype',
    name: '',
    description: ''
  };

  S.sender('new-doctype-built', JSON.stringify(obj));

  return 'doctype-sent-to-editor';
};

exports.init = init;
exports.get = get;
exports.prefix = prefix;
exports.addDoctype = addDoctype;
