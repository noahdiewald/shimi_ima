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

function getIndex(startkey, startid, prevkeys, previds) {
  var url = "documents/index?";
  var query = $('#index-query').val();
  var limit = $('#index-limit').val() * 1;
  
  // Initialize some values if we're at the beginning of the listing
  if (!prevkeys) {
    startkey = btoa($('#index-filter').val());
    prevkeys = [];
    previds = [];
  }
  
  if (startkey) {
    url = url + '&startkey=' + escape(atob(startkey));
    
    if (startid) {
      url = url + '&startkey_docid=' + startid;
    }
  }
  
  // The user supplied limit will need a plus one so that we can
  // get the start key for the next page from the server.
  if (limit) {
    url = url + '&limit=' + (limit + 1);
  } else {
    // Ten is the default and I don't let people leave it blank
    // because the list could be huge.
    $('#index-limit').val(10);
    url = url + '&limit=11';
  }
  
  if (query) {
    url = url + '&query=' + query;
  }
  
  $.get(url, function(documentIndexHtml) {
    $('#document-index #index-listing').html(documentIndexHtml);
    
    $('#previous-index-page').button({
      icons: {primary:'ui-icon-circle-arrow-w'} 
    }).click(function() {
      getIndex(prevkeys.pop(), previds.pop(), prevkeys, previds);
    });
    
    // Collect the values needed for paging from the HTML
    $('#next-index-page').button({
      icons: {secondary:'ui-icon-circle-arrow-e'}
    }).click(function() {
      var nextkey = $(this).attr('data-startkey');
      var nextid = $(this).attr('data-startid');
      var prevkey = $('#first-index-element').attr('data-first-key');
      var previd = $('#first-index-element').attr('data-first-id');
      prevkeys.push(prevkey);
      previds.push(previd);
      
      getIndex(nextkey, nextid, prevkeys, previds);
    });
    
    // Disable the previous button if we're at the beginning
    if (prevkeys.length == 0) {
      $('#previous-index-page').button("disable");
    }
    
    // Disable the next button if we're at the end
    if ($('#next-index-page').attr('data-last-page')) {
      $('#next-index-page').button("disable");
    }
  
    $('nav.pager').buttonset();
    
    // Allows the document for the listed item to be displayed
    // in the correct pane on click.
    $('.view-document-link').click(function() {
      getDocument(this.hash.slice(1));
    });
  });
}

function fillQueryOptions() {
  url = "/projects/project-" + $('#container').attr('data-project-id') +
        "/queries?as=options"
        
  $.get(url, function(data) {
    $('#index-query').html(data);
  });
}
