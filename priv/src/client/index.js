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
shimi.index = function (args)
{
  'use strict';

  var mod = {};
  if (args.prefix === undefined)
  {
    args.prefix = 'index';
  }
  var prefix = args.prefix;

  mod.get = function (startkey, startid, prevkeys, previds)
  {
    var url = args.url + '?';
    var indexId = args.indexId;
    var limitField = $('#' + prefix + '-limit');
    var limit = limitField.val() * 1;
    var target = args.target;
    var filterVal = JSON.stringify($('#' + prefix + '-filter').val());
    var state = {
      sk: startkey,
      sid: startid,
      pks: prevkeys,
      pids: previds
    };

    if (!state.pks)
    {
      state.sk = window.btoa(window.unescape(window.encodeURIComponent(filterVal)));
      state.pks = [];
      state.pids = [];
    }

    if (state.sk)
    {
      url = url + '&startkey=' + window.escape(window.atob(state.sk));
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
      limitField.val(25);
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
    target.html(req.responseText);

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

    return mod;
  };

  return mod;
};
