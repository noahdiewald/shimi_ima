// # Paging For Changes Listing
//
// *Implicit depends:* DOM, JQuery
//
// Loads changes based on user suplied values.

// Variable Definitions

var pager = require('../pager.js').pager;

// Exported Functions

// Return the 'prefix' which is used in id and class names for
// elements used to page through these values.
var prefix = function ()
{
  'use strict';

  return 'changelog';
};

// Called by a keystroke event handler when user changes form values.
var get = function ()
{
  'use strict';

  var prefix = prefix();
  var url = prefix;
  var target = $('#' + prefix + '-listing');

  var format = function (text)
  {
    var resp = JSON.parse(text);

    resp.rows.map(function (item)
    {
      if (item.doc.changes)
      {
        item.doc.changes = Object.keys(item.doc.changes).map(function (key)
        {
          return item.doc.changes[key];
        });
      }
    });

    return resp;
  };

  pager(
  {
    prefix: 'changelog',
    url: url,
    format: format,
    target: target
  }).get();

  return true;
};

exports.prefix = prefix;
exports.get = get;
