// # Paging List-like Info
//
// *Implicit depends:* DOM, JSON
//
// This is basically semi-generic paging code.
//
// Get the index that is displayed in the index pane.  startkey and
// startid map directly to the same concepts in couchdb view queries. The
// prevkeys and previds are used to hold information that will allow
// the user to page backward through the listing. They are arrays of
// keys and ids corresponding to previous page's startkeys and ids.
//
// There are a number of values that this function depends on that
// are taken from the HTML. These include the value for the limit and
// the nextkey and nextid for paging forward. Also the current key and
// id are taken from the html when needed to add to the prevkeys and
// previds. The startkey may be a user input value so a more reliable
// startkey and startid are needed.

// Variable Definitions

var form = require('./form.js');

// Exported functions

// Initialize the pager with an args object.
var pager = function (args)
{
  'use strict';

  var mod = {};
  // If the 'prefix' used to automatically determine certain element
  // ID's is not set, set it to 'index'.
  if (args.prefix === undefined)
  {
    args.prefix = 'index';
  }
  // Special formatting or template code.
  var format = args.format;
  var prefix = args.prefix;

  // Escape a value and base64 encode it.
  var escapeValue = function (value)
  {
    return window.btoa(window.unescape(window.encodeURIComponent(JSON.stringify(value))));
  };

  // The number of elements to display is given here. Note how `prefix`
  // is used.
  var limitField = function ()
  {
    return document.getElementById(prefix + '-limit');
  };

  // Get the first or next page. There won't be `prevkeys` or `previds`
  // if it is the first page. These accumulate during paging so that it
  // is possible to go backwards.
  mod.get = function (startkey, startid, prevkeys, previds)
  {
    // The URL given as one of the original args.
    var url = args.url + '?';
    // This would be a custom index ID.
    var indexId = args.indexId;
    // The given limit.
    var limit = limitField().value * 1;
    // Where the next page will be displayed.
    var target = args.target;
    // The filter is used to constrain the values listed.
    var filterVal = document.getElementById(prefix + '-filter').value;
    var state = {
      sk: startkey,
      sid: startid,
      pks: prevkeys,
      pids: previds
    };

    if (!state.pks)
    {
      state.sk = escapeValue(filterVal);
      state.pks = [];
      state.pids = [];
    }

    if (state.sk)
    {
      url = url + 'startkey=' + window.escape(window.atob(state.sk));
      if (state.sid)
      {
        url = url + '&startkey_docid=' + state.sid;
      }
    }

    if (limit)
    {
      url = url + '&limit=' + (limit + 1);
    }
    else
    {
      limitField().value = 25;
      url = url + '&limit=26';
    }

    if (indexId)
    {
      url = url + '&index=' + indexId;
    }

    form.send(url, false, 'GET', function (context, req)
    {
      mod.fill(req, state, target);
    }, this);

    return mod;
  };

  mod.fill = function (req, state, target)
  {
    var limit = limitField().value * 1;
    var respJSON;
    var lastrow;
    var newRows;

    var prevElem = function ()
    {
      return document.getElementById('previous-' + prefix + '-page');
    };

    var nextElem = function ()
    {
      return document.getElementById('next-' + prefix + '-page');
    };

    var prevHandler = function ()
    {
      mod.get(state.pks.pop(), state.pids.pop(), state.pks, state.pids);
    };

    var nextHandler = function ()
    {
      var firstElem = document.getElementById('first-' + prefix + '-element');
      var nextkey = nextElem().getAttribute('data-startkey');
      var nextid = nextElem().getAttribute('data-startid');
      var prevkey = firstElem.getAttribute('data-first-key');
      var previd = firstElem.getAttribute('data-first-id');
      state.pks.push(prevkey);
      state.pids.push(previd);

      mod.get(nextkey, nextid, state.pks, state.pids);
    };

    if (format === undefined)
    {
      respJSON = JSON.parse(req.responseText);
    }
    else
    {
      respJSON = format(req.responseText);
    }

    newRows = respJSON.rows.map(function (item, index, thisArray)
    {
      item.encoded_key = escapeValue(item.key);
      return item;
    });

    lastrow = newRows.slice(-1);

    if (newRows[0])
    {
      newRows[0].firstrow = true;
    }

    if (newRows.length > limit)
    {
      respJSON.rows = newRows.slice(0, -1);
    }
    else
    {
      respJSON.rows = newRows;
      respJSON.lastpage = true;
    }

    respJSON.lastrow = lastrow;
    respJSON.prefix = prefix;

    target.innerHTML = templates['paged-listing'].render(respJSON,
    {
      'listed-element': templates[prefix + '-element']
    });

    nextElem().onclick = nextHandler;
    prevElem().onclick = prevHandler;

    // Disable the previous button if we're at the beginning
    if (state.pks.length === 0)
    {
      prevElem().classList.add('hidden');
    }

    // Disable the next button if we're at the end
    if (nextElem().getAttribute('data-last-page'))
    {
      nextElem().classList.add('hidden');
    }

    return mod;
  };

  return mod;
};

exports.pager = pager;
