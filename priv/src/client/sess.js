// # Session storage helpers
//
// *Implicit depends:* DOM
//
// This is primarily used to store and retrieve items with a structure
// similar to a CouchDB document.

// Exported functions

// Like put but will overwrite the previous item.
var replace = function (doc) {
  'use strict';

  window.sessionStorage[doc._id] = JSON.stringify(doc);

  return doc._id;
};

// If the item is not already in the session storage, convert it to JSON
// and store it by `_id`. Return the `_id` of the document.
var put = function (doc) {
  'use strict';

  if (!window.sessionStorage[doc._id]) {
    replace(doc);
  }

  return doc._id;
};

// Retrieve the document, which is stored as JSON, by its `_id` and
// return the parsed item. If the item does not exist, return `null`.
var get = function (docId) {
  'use strict';

  var doc = window.sessionStorage[docId];

  if (doc) {
    return JSON.parse(doc);
  } else {
    return null;
  }
};

exports.replace = replace;
exports.put = put;
exports.get = get;
