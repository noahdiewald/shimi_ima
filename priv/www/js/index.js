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

var index = function(args) {
  var mod = {};
  
  mod.url = args.url + '?';
  mod.indexId = args.indexId;
  mod.limitField = $('#index-limit');
  mod.limit = limitField.val() * 1;
  mod.target = args.target;
  mod.state = {};

  mod.get = function(startkey, startid, prevkeys, previds) {
    var filterVal = JSON.stringify($('#index-filter').val());
    mod.state = {
      sk: startkey,
      sid: startid,
      pks: prevkeys,
      pids: previds
    };

    if (!mod.state.pks) {
      mod.state.sk = btoa(unescape(encodeURIComponent(json_encoded)));
      mod.state.pks = [];
      mod.state.pids = [];
    }

    if (mod.state.sk) {
      mod.url = mod.url + '&startkey=' + escape(atob(mod.state.sk));
      if (mod.state.sid) {
        mod.url = mod.url + '&startkey_docid=' + mod.state.sid;
      }
    }

    if (mod.limit) {
      mod.url = mod.url + '&limit=' + (mod.limit + 1);
    } else {
      mod.limitField.val(25);
      mod.url = mod.url + '&limit=26';
    }

    if (mod.indexId) {
      mod.url = mod.url + '&index=' + mod.indexId;
    }

    sendConfigDoc(url, fals, 'GET',
                  function(context, req) {mod.fill(req);}, this);

    return mod;
  };

  mod.fill = function(req) {
    mod.target.html(req.responseText);
  
    $('#previous-index-page').button(
      {
        icons: {primary:'ui-icon-circle-arrow-w'} 
      }).click(function() 
               {
                 mod.get(state.pks.pop(), 
                         state.pids.pop(), 
                         state.pks, 
                         state.pids);
               });

    $('#next-index-page').button(
      {
        icons: {secondary:'ui-icon-circle-arrow-e'}
      }).click(function(e) 
               {
                 var nextkey = $(e).attr('data-startkey');
                 var nextid = $(e).attr('data-startid');
                 var prevkey = 
                   $('#first-index-element').attr('data-first-key');
                 var previd = 
                   $('#first-index-element').attr('data-first-id');
                 state.pks.push(prevkey);
                 state.pids.push(previd);
                 
                 mod.get(nextkey, nextid, state.prevkeys, state.previds);
               });
    
    // Disable the previous button if we're at the beginning
    if (state.pks.length == 0) {
      $('#previous-index-page').button("disable");
    }
    
    // Disable the next button if we're at the end
    if ($('#next-index-page').attr('data-last-page')) {
      $('#next-index-page').button("disable");
    }
    
    $('nav.pager').buttonset();

    return mod;
  };
};