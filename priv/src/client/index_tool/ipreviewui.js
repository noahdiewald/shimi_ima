// # Paging For Index Listing
//
// *Implicit depends:* DOM, JQuery
//
// Loads sample of the user index based on user suplied values.

// Variable Definitions

var pager = require('../pager.js').pager;

// Exported Functions

// Called by a keystroke event handler when user changes form values.
var get = function ()
{
  var prefix = 'preview';
  var indexId = $('#index-editing-data').attr('data-index-id');
  var url = 'indexes/' + indexId + '/preview';
  var target = $('#' + prefix + '-list-view');

  var format = function (text)
  {
    var resp = JSON.parse(text);

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
      prefix: prefix,
      format: format,
      url: url,
      target: target
    }).get(startkey, startid, prevkeys, previds);
  }

  return true;
};

exports(get);
