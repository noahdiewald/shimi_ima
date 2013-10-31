// # Paging For Index Listing
//
// *Implicit depends:* DOM, JSON
//
// Loads sample of the user index based on user suplied values.

// Variable Definitions

var pager = require('../pager.js').pager;

// Exported Functions

// Return the 'prefix' which is used in id and class names for
// elements used to page through these values.
var prefix = function ()
{
  'use strict';

  return 'preview';
};

// Called by a keystroke event handler when user changes form values.
var get = function ()
{
  'use strict';

  var indexId = document.getElementById('index-editing-data').getAttribute('data-index-id');
  var url = 'indexes/' + indexId + '/preview';
  var target = document.getElementById(prefix() + '-list-view');

  var format = function (resp)
  {
    resp.rows = resp.rows.map(function (item)
    {
      item.display_key = item.key.map(function (k)
      {
        return k[1];
      });

      return item;
    });

    return resp;
  };

  if (indexId)
  {
    pager(
    {
      prefix: prefix(),
      format: format,
      url: url,
      target: target
    }).get();
  }

  return true;
};

exports.prefix = prefix;
exports.get = get;
