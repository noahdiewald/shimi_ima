function initTabs() {
  $("#main-tabs").tabs();
  return false;
}

function setQueryDoctypeEvents(queryDoctype, queryFieldset) {
  queryDoctype.change(function() {
    var url = 'doctypes/' + queryDoctype.val() + '/fieldsets';
    
    fillOptionsFromUrl(url, queryFieldset);
  });
  
  return false;
}

function getFieldDoc(fieldId, fieldsetId, doctypeId, callback) {
  var fieldDoc;
  var url = 'doctypes/' + doctypeId + 
            '/fieldsets/' + fieldsetId + 
            '/fields/' + fieldId + '?format=json';
            
  if (fieldDoc = getDoc(fieldId)) {
    if (callback) {
      callback(fieldDoc);
    }
    return fieldDoc;
  } else {
    $.getJSON(url, function(data) {
      putDoc(data);
      if (callback) {
        callback(getDoc(fieldId));
      }
    });
    
    return getDoc(fieldId);
  }
}

function fillOptionsFromUrl(url, selectElement) {
  $.get(url, function(options) {
    selectElement.html(options);
  });
  
  return false;
}

function alterOperatorField(fieldDoc, fieldId) {
  disableOperatorOptions(fieldDoc);
  
  return false;
}

function disableOperatorOptions(fieldDoc) {
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
}

function disableOptions(options, disables) {
  options.children().show();
  
  disables.forEach(function(item) {
    options.children('option:contains(' + item + ')').hide();
  });
  
  return false;
}

function alterArgumentField(argumentField, operatorField, fieldField) {
  var fieldDoc = function () {return getDoc(fieldField.val())};
  
  argumentField.removeAttr('disabled').datepicker('destroy');
  argumentField.removeAttr('disabled').autocomplete('destroy');
  
  function dateOrText(argumentField, fdoc) {
    if (fdoc.subcategory == 'date') {
      argumentField.removeAttr('disabled');
      argumentField.datepicker({dateFormat: "yy-mm-dd"});
    } else {
      argumentField.removeAttr('disabled');
      argumentField.autocomplete({source: fdoc.allowed});
    }
    
    return false;
  }
  
  if (fdoc = fieldDoc()) {
    switch (operatorField.val()) {
      case "true":
      case "blank":
        argumentField.attr('disabled', 'disabled').val("");
        break;
      case "equal":
      case "member":
      case "greater":
      case "less":
        dateOrText(argumentField, fdoc);
        break;
    }
    
  }
}

function fixArgumentType(argument, subcategory) {
  switch (subcategory) {
    case "integer":
    case "rational":
      argument = argument * 1;
      break;
  }
  
  return argument;
}

function getQueryConditions(doctypeId, rows) {
  var conditions = rows.map(function(index, row) {
    row = $(row);
    var fieldId = row.find('td.field-condition').attr('data-value');
    var fieldsetId = row.find('td.fieldset-condition').attr('data-value');
    var argument = row.find('td.argument-condition').attr('data-value');
    var fieldDoc = getFieldDoc(fieldId, fieldsetId, doctypeId);
    var is_or = row.find('td.or-condition').attr('data-value') == "true";
    var negate = row.find('td.negate-condition').attr('data-value') == "true";
    var operator = row.find('td.operator-condition').attr('data-value');
    var condition;
    
    if (is_or) {
      condition = { "is_or": true };
    } else {
      argument = fixArgumentType(argument, fieldDoc.subcategory);
      
      var condition = {
        "is_or": false,
        "negate": negate,
        "fieldset": fieldsetId,
        "field": fieldId,
        "operator": operator,
        "argument": argument
      };
    }
    
    return condition;
  }).toArray();
  
  return conditions;
}

function saveQuery(buttonData, completeFunction) {
  var queryId = buttonData.attr('data-query-id');
  var queryRev = buttonData.attr('data-query-rev');
  var url = "queries/" + queryId + "?rev=" + queryRev;
  var doctype = buttonData.attr('data-query-doctype');
  
  var obj = {
    "_id": queryId,
    "category": "query",
    "doctype": doctype,
    "fieldset": buttonData.attr('data-query-fieldset'),
    "field": buttonData.attr('data-query-field'),
    "name": buttonData.attr('data-query-name'),
    "conditions": getQueryConditions(doctype, $('#query-conditions-listing tbody tr'))
  };
  
  sendConfigDoc(url, obj, 'PUT', completeFunction, this);

  return false;  
}

function deleteQuery(queryId, queryRev, completeMessage, completeFunction) {
  var url = "queries/" + queryId + "?rev=" + queryRev;
  
  $.ajax({
    type: "DELETE",
    url: url,
    dataType: "json",
    contentType: "application/json",
    complete: function(req, status) {
      if (req.status == 204) {
        var title = "Success";
        var body = completeMessage;
        
        completeFunction();
        
        flashHighlight(title, body);
      } else if (req.status == 409) {
        var body = JSON.parse(req.responseText);
        var title = req.statusText;
          
        flashError(title, body.message);
      } else if (req.status == 404) {
        var body = "Query appears to have been deleted already.";
        var title = req.statusText;
          
        flashError(title, body);
      }
    }
  });

  return false;  
}

function initQueryEditButtons(buttonData) {
  initQuerySaveButton($('#save-query-button'), buttonData);
  initQueryDeleteButton($('#delete-query-button'), buttonData);
  initQueryAddConditionButton($('#add-query-condition-button'), buttonData);
  
  return false;
}
 
function initQuerySaveButton(button, buttonData) {
  button.button({
    icons: {primary: "ui-icon-document"}
  }).click(function (e) {
    var bData = buttonData();
    
    if (!bData.length < 1) {
      var completeFunction = function() {
        getQueryEdit(bData.attr('data-query-id'));
        flashHighlight("Success", "Your query has been saved.");
      };
      
      saveQuery(bData, completeFunction);
    } else {
      flashHighlight("Info", "No query has been chosen to save.");
    }
  });
}

function getQueryEdit(queryId) {
  var url = "queries/" + queryId;
  var target = $('#query-edit');
  
  $.get(url, function(queryData) {
    target.html(queryData);
    // TODO don't repeat this code. It is also in initQueryBuilderDialog
    tableBody = $('#query-conditions-listing tbody');
    tableBody.sortable();
    initConditionRemoveButtons(tableBody);
    getQueryView();
  });
  
  return false;
}

function initQueryIndex() {
  var url = "queries";
  var target = $('#query-index-listing');
  
  $.get(url, function(index) {
    target.html(index);
    target.click(function(e) {
      getQueryEdit($(e.target).attr('data-query-id'));
      target.slideToggle();
    });
  });
}

function getQueryView(startkey, startid, prevkeys, previds) {
  var queryInfo = $('#query-editing-data');
  var queryId = queryInfo.attr('data-query-id');
  var url = "queries/" + queryId + "/view?";
  var limit = $('#query-limit').val() * 1;
  
  // Initialize some values if we're at the beginning of the listing
  if (!prevkeys) {
    startkey = $('#query-filter').val();
    prevkeys = [];
    previds = [];
  }
  
  if (startkey) {
    url = url + '&startkey=' + encodeURIComponent(JSON.stringify([startkey]));
    
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
    $('#query-limit').val(10);
    url = url + '&limit=11';
  }
  
  $.get(url, function(data) {
    $('#query-list-view').html(data);
    
    $('#previous-page').button({
      icons: {primary:'ui-icon-circle-arrow-w'} 
    }).click(function() {
      getQueryView(prevkeys.pop(), previds.pop(), prevkeys, previds);
    });
    
    // Collect the values needed for paging from the HTML
    $('#next-page').button({
      icons: {secondary:'ui-icon-circle-arrow-e'}
    }).click(function() {
      var nextkey = $(this).attr('data-startkey');
      var nextid = $(this).attr('data-startid');
      var prevkey = $('#first-index-element').attr('data-first-key');
      var previd = $('#first-index-element').attr('data-first-id');
      prevkeys.push(prevkey);
      previds.push(previd);
      
      getQueryView(nextkey, nextid, prevkeys, previds);
    });
    
    // Disable the previous button if we're at the beginning
    if (prevkeys.length == 0) {
      $('#previous-page').button("disable");
    }
    
    // Disable the next button if we're at the end
    if ($('#next-page').attr('data-last-page')) {
      $('#next-page').button("disable");
    }
  
    $('nav.pager').buttonset();
    
  });

}

$(function () {
  initTabs(); 
  $('#query-builder-dialog').hide();
  $('#query-new-dialog').hide();
  initQueryEditButtons(function () {return $('#query-editing-data')});
  initQueryNewButton();
  initQueryChooseButton();
  $('#button-bar').buttonset();
  initQueryIndex();
  $('#query-filter-form input').keyup(function() {
    getQueryView();
  });
});
