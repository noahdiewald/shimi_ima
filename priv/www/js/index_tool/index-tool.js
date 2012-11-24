var getFieldDoc = function(fieldId, fieldsetId, doctypeId, callback) {
  var fieldDoc = getDoc(fieldId);
  var url = 'doctypes/' + doctypeId + 
    '/fieldsets/' + fieldsetId + 
    '/fields/' + fieldId + '?format=json';
            
  if (fieldDoc) {
    if (callback) {
      callback(fieldDoc);
    }
    return fieldDoc;
  } else {
    $.ajax({
             url: url,
             async: false,
             dataType: 'json',
             success: function(data) {
               putDoc(data);
               if (callback) {
                 callback(getDoc(fieldId));
               }
             }
           });
          
    return getDoc(fieldId);
  }
};

var fillOptionsFromUrl = function(url, selectElement, callback) {
  $.get(url, function(options) {
          selectElement.html(options);
          if (callback) callback();
        });
  
  return false;
};

var alterOperatorField = function(fieldDoc, fieldId, callback) {
  disableOperatorOptions(fieldDoc);
  callback();
  
  return false;
};

var disableOperatorOptions = function(fieldDoc) {
  var options = $('#builder-operator-input');
  
  switch (fieldDoc.subcategory) {
  case "select":
  case "docselect":
  case "text":
  case "textarea":
    disableOptions(options, ["member", "true"]);
    break;
  case "integer":
  case "rational":
  case "date":
    disableOptions(options, ["member", "true", "match"]);
    break;
  case "boolean":
  case "openboolean":
    disableOptions(options, ["equal", "greater", "less", "member", "match"]);
    break;
  case "multiselect":
  case "docmultiselect":
    disableOptions(options, ["equal", "greater", "less", "true", "match"]);
    break;
  }
  
  return false;
};

var disableOptions = function(options, disables) {
  options.children().show();
  
  disables.forEach(function(item) {
                     options.children('option:contains(' + item + ')').hide();
                   });
  
  return false;
};

var alterArgumentField = 
  function(argumentField, operatorField, fieldField, callback) {
    var fieldDoc = function () {return getDoc(fieldField.val());};

    callback();
  
    argumentField.removeAttr('disabled').datepicker('destroy');
    argumentField.removeAttr('disabled').autocomplete('destroy');
  
    var dateOrText = function(argumentField, fdoc) {
      if (fdoc.subcategory == 'date') {
        argumentField.removeAttr('disabled');
        argumentField.datepicker({dateFormat: "yy-mm-dd"});
      } else {
        argumentField.removeAttr('disabled');
        argumentField.autocomplete({source: fdoc.allowed});
      }
    
      return false;
    };

    var fdoc = fieldDoc();
  
    if (fdoc) {
      switch (operatorField.val()) {
      case "true":
      case "isDefined":
      case "blank":
        argumentField.attr('disabled', 'disabled').val("");
        break;
      case "equal":
      case "member":
      case "greater":
      case "less":
      case "hasExactly":
      case "hasGreater":
      case "hasLess":
        dateOrText(argumentField, fdoc);
        break;
      }
    
    }
  };

var getIndexView = function(startkey, startid, prevkeys, previds) {
  var state = {
    startkey: startkey,
    startid: startid,
    prevkeys: prevkeys,
    previds: previds
  };
  var indexInfo = $('#index-editing-data');
  var indexId = indexInfo.attr('data-index-id');
  var url = "indexes/" + indexId + "/view?";
  var limit = $('#index-limit').val() * 1;
  
  // Initialize some values if we're at the beginning of the listing
  if (!state.prevkeys) {
    var supplied_val = $('#index-filter').val();
    var json_encoded = JSON.stringify(supplied_val);
    state.startkey = btoa(unescape(encodeURIComponent(json_encoded)));
    state.prevkeys = [];
    state.previds = [];
  }
  
  if (state.startkey) {
    url = url + '&startkey=' + escape(atob(state.startkey));
    
    if (state.startid) {
      url = url + '&startkey_docid=' + state.startid;
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

  sendConfigDoc(url, false, 'GET', 
                function(context, req) 
                {fillIndexPreview(req, state);}, this);  
};
  
var fillIndexPreview = function(req, state) {
  $('#index-list-view').html(req.responseText);
  
  $('#previous-page').button(
    {
      icons: {primary:'ui-icon-circle-arrow-w'} 
    }).click(function() 
             {
               getIndexView(state.prevkeys.pop(), 
                            state.previds.pop(), 
                            state.prevkeys, 
                            state.previds);
             });
    
  // Collect the values needed for paging from the HTML
  $('#next-page').button(
    {
      icons: {secondary:'ui-icon-circle-arrow-e'}
    }).click(function() 
             {
               var nextkey = $(this).attr('data-startkey');
               var nextid = $(this).attr('data-startid');
               var prevkey = 
                 $('#first-index-element').attr('data-first-key');
               var previd = 
                 $('#first-index-element').attr('data-first-id');
               state.prevkeys.push(prevkey);
               state.previds.push(previd);
                       
               getIndexView(nextkey, nextid, state.prevkeys, state.previds);
             });
    
  // Disable the previous button if we're at the beginning
  if (state.prevkeys.length == 0) {
    $('#previous-page').button("disable");
  }
    
  // Disable the next button if we're at the end
  if ($('#next-page').attr('data-last-page')) {
    $('#next-page').button("disable");
  }
  
  $('nav.pager').buttonset();
  
};
  
  var initIndexIndex = function() {
    var url = "indexes";
    var target = $('#index-index-listing');
    
    $.get(url, function(index) {
            target.html(index);
            target.click(function(e) {
                           getIndexEdit($(e.target).attr('data-index-id'));
                         });
          });
  };

$(function () {
    $('#index-builder-dialog').hide();
    $('#index-new-dialog').hide();
    $('#index-replace-dialog').hide();
    initIndexEditButtons(function () {return $('#index-editing-data');});
    initIndexNewButton();
    $('#button-bar').buttonset();
    initIndexIndex();
    $('#index-filter-form input').keyup(function() {
                                          getIndexView();
                                        });
  });
