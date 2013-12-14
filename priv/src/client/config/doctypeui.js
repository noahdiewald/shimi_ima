// # Doctype Listing
//
// *Implicit depends:* DOM
//
// Loads a listing of doctypes that can be edited.

var templates = require('templates.js');
var pager = require('../pager.js').pager;

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

var init = function () {
  'use strict';

  get();

  return 'doctypeui-initialized';
};

exports.init = init;
exports.get = get;
exports.prefix = prefix;
