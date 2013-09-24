// # Index Listing
//
// *Implicit depends:* DOM, JSON, JQuery
//
// Loads index based on user suplied values. It also loads some other
// preliminary data, such as the listing of user created indexes. The
// `load()` function performs some initialization.

// ## Variable Definitions

var templates = require('templates.js');
var pager = require('../pager.js').pager;
var viewui = require('./viewui.js');
var editui = require('./editui.js');

// ## Exported Functions

// Return the 'prefix' which is used in id and class names for
// elements used to page through these values.
var prefix = function ()
{
  'use strict';

  return 'index';
};

// Called by a keystroke event handler when user changes form values.
var get = function ()
{
  'use strict';

  var url = 'documents/' + prefix();
  var indexId = document.getElementById('index-' + prefix() + '-input').value;
  var target = document.getElementById(prefix() + '-listing');

  var format = function (text)
  {
    var resp = JSON.parse(text);

    resp.rows = resp.rows.map(function (item)
    {
      item.display_key = item.key.map(function (k)
      {
        return k[1];
      });

      if (indexId && item.value.length > 0)
      {
        item.value = item.value.split(', ');
      }

      return item;
    });

    return resp;
  };

  pager(
  {
    prefix: prefix(),
    format: format,
    url: url,
    indexId: indexId,
    target: target
  }).get();

  return true;
};

// Loads the listing of user created indexes.
var iOpts = function ()
{
  'use strict';

  var url = 'indexes?as=options';
  var options;

  $.getJSON(url, function (data)
  {
    options = templates['index-options'](data);
    $('#index-index-input').html(options);
  });

  return true;
};

// This is the entry point that loads the data for this section of
// the application.
//
// TODO: Move to documents.js
var load = function (target)
{
  'use strict';

  var id = $(target).attr('href').slice(1);
  $('#document-view').html('<em>Loading...</em>');
  editui.clear();
  viewui.get(id);

  return true;
};

exports.prefix = prefix;
exports.get = get;
exports.iOpts = iOpts;
exports.load = load;
