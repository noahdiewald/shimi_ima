// # Charseq Listing
//
// *Implicit depends:* DOM
//
// Loads a listing of charseqs that can be edited.

var templates = require('templates');
var pager = require('pager').pager;

// ## Exported Functions

// Return the 'prefix' which is used in id and class names for
// elements used to page through these values.
var prefix = function () {
  'use strict';

  return 'charseqs';
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

  return 'charseq-listing-requested';
};

var init = function () {
  'use strict';

  get();

  return 'charsequi-initialized';
};

exports.init = init;
exports.get = get;
exports.prefix = prefix;
