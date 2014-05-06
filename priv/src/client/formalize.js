// # Formalize
//
// Convert JSON to and from an HTML form.

// ## Variable Definitions

var formalize_from = require('formalize_from');
var formalize_to = require('formalize_to');
// ## External Functions

var toForm = function (json, options) {
  'use strict';

  return formalize_to.transform(json, options);
};

var fromForm = function (html) {
  'use strict';

  return formalize_from.transform(html);
};

exports.toForm = toForm;
exports.fromForm = fromForm;
