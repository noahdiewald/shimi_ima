// Get the index that is displayed in the index pane.
// startkey and startid map directly to the same concepts in
// couchdb view queries. The prevkeys and previds are used to
// hold information that will allow the user to page backward
// through the listing. They are arrays of keys and ids corresponding
// to previous page's startkeys and ids.
//
// There are a number of values that this function depends on
// that are taken from the HTML. These include the value for
// the limit and the nextkey and nextid for paging forward. Also
// the current key and id are taken from the html when needed to
// add to the prevkeys and previds. The startkey may be a user
// input value so a more reliable startkey and startid are needed.
shimi.pager = function (args)
{
  'use strict';

  var mod = {};
  if (args.prefix === undefined)
  {
    args.prefix = 'index';
  }
  var origin = args.origin;
  var format = args.format;
  var prefix = args.prefix;

  var escapeValue = function (value)
  {
    return window.btoa(window.unescape(window.encodeURIComponent(JSON.stringify(value))));
  };

  var limitField = function ()
  {
    return $('#' + prefix + '-limit');
  };

  mod.get = function (startkey, startid, prevkeys, previds)
  {
    var url = args.url + '?';
    var indexId = args.indexId;
    var limit = limitField().val() * 1;
    var target = args.target;
    var filterVal = $('#' + prefix + '-filter').val();
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
      limitField().val(25);
      url = url + '&limit=26';
    }

    if (indexId)
    {
      url = url + '&index=' + indexId;
    }

    shimi.form.send(url, false, 'GET', function (context, req)
    {
      mod.fill(req, state, target);
    }, this);

    return mod;
  };

  mod.fill = function (req, state, target)
  {
    var limit = limitField().val() * 1;

    var respJSON;
    var lastrow;
    var newRows;

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

    target.html(templates['paged-listing'].render(respJSON,
    {
      'listed-element': templates[prefix + '-element']
    }));

    $('#previous-' + prefix + '-page').click(function ()
    {
      mod.get(state.pks.pop(), state.pids.pop(), state.pks, state.pids);
    });

    $('#next-' + prefix + '-page').click(function ()
    {
      var nextkey = $('#next-' + prefix + '-page').attr('data-startkey');
      var nextid = $('#next-' + prefix + '-page').attr('data-startid');
      var prevkey = $('#first-' + prefix + '-element').attr('data-first-key');
      var previd = $('#first-' + prefix + '-element').attr('data-first-id');
      state.pks.push(prevkey);
      state.pids.push(previd);

      mod.get(nextkey, nextid, state.pks, state.pids);
    });

    // Disable the previous button if we're at the beginning
    if (state.pks.length === 0)
    {
      $('#previous-' + prefix + '-page').hide();
    }

    // Disable the next button if we're at the end
    if ($('#next-' + prefix + '-page').attr('data-last-page'))
    {
      $('#next-' + prefix + '-page').hide();
    }

    var keyupHandler = function (e)
    {
      var getIndexTimer;
      window.clearTimeout(getIndexTimer);
      getIndexTimer = setTimeout(function ()
      {
        if (e.which !== 8 && e.which !== 46)
        {
          shimi[origin].get();
        }
      }, 500);
    };

    document.getElementById(prefix + '-filter').onkeyup = keyupHandler;
    document.getElementById(prefix + '-limit').onkeyup = keyupHandler;

    return mod;
  };

  return mod;
};
