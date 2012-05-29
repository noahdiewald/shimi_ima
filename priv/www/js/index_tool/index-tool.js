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
  };

var fixArgumentType = function(argument, subcategory) {
  switch (subcategory) {
  case "integer":
  case "rational":
    argument = argument * 1;
    break;
  }
  
  return argument;
};

var getIndexConditions = function(doctypeId, rows) {
  var conditions = rows.map(
    function(index, row) {
      row = $(row);
      var is_or = row.find('td.or-condition').attr('data-value') == "true";
      var condition;
    
      if (is_or) {
        condition = { "is_or": true };
      } else {
        var fieldId = row.find('td.field-condition').attr('data-value');
        var fieldsetId = row.find('td.fieldset-condition').attr('data-value');
        var argument = row.find('td.argument-condition').attr('data-value');
        var fieldDoc = getFieldDoc(fieldId, fieldsetId, doctypeId);
        var negate = 
          row.find('td.negate-condition').attr('data-value') == "true";
        var operator = row.find('td.operator-condition').attr('data-value');

        argument = fixArgumentType(argument, fieldDoc.subcategory);
      
        condition = {
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
};

var saveIndex = function(buttonData, completeFunction) {
  var indexId = buttonData.attr('data-index-id');
  var indexRev = buttonData.attr('data-index-rev');
  var url = "indexes/" + indexId + "?rev=" + indexRev;
  var doctype = buttonData.attr('data-index-doctype');
  
  var obj = {
    "_id": indexId,
    "category": "index",
    "doctype": doctype,
    "show_deleted": buttonData.attr('data-index-show_deleted') === "true",
    "fields": JSON.parse(buttonData.attr('data-index-fields')),
    "fields_label": JSON.parse(buttonData.attr('data-index-fields_label')),
    "name": buttonData.attr('data-index-name'),
    "conditions": getIndexConditions(doctype, $('#index-conditions-listing tbody tr'))
  };
  
  sendConfigDoc(url, obj, 'PUT', completeFunction, this);

  return false;  
};

var deleteIndex = 
  function(indexId, indexRev, completeMessage, completeFunction) {
    var url = "indexes/" + indexId + "?rev=" + indexRev;
  
    $.ajax(
      {
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
            var body = "Index appears to have been deleted already.";
            var title = req.statusText;
            
            flashError(title, body);
          }
        }
      });

    return false;  
  };

var initIndexEditButtons = function(buttonData) {
  initIndexSaveButton($('#save-index-button'), buttonData);
  initIndexDeleteButton($('#delete-index-button'), buttonData);
  initIndexAddConditionButton($('#add-index-condition-button'), buttonData);
  
  return false;
};
 
var initIndexSaveButton = function(button, buttonData) {
  var completeFunction;
  var bData;
  
  button.button(
    {
      icons: {primary: "ui-icon-document"}
    }).click(function (e) {
               bData = buttonData();
    
               if (!bData.length < 1) {
                 completeFunction = function() {
                   getIndexEdit(bData.attr('data-index-id'));
                   flashHighlight("Success", "Your index has been saved.");
                 };
      
                 saveIndex(bData, completeFunction);
               } else {
                 flashHighlight("Info", "No index has been chosen to save.");
               }
             });
};

var getIndexEdit = function(indexId) {
  var url = "indexes/" + indexId;
  var target = $('#index-conditions');
  
  $.get(url, function(indexData) {
          target.html(indexData);
          // TODO don't repeat this code. It is also in initIndexBuilderDialog
          var tableBody = $('#index-conditions-listing tbody');
          tableBody.sortable();
          initConditionRemoveButtons(tableBody);
          getIndexView();
        });
  
  return false;
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

var getIndexView = function(startkey, startid, prevkeys, previds) {
  var indexInfo = $('#index-editing-data');
  var indexId = indexInfo.attr('data-index-id');
  var url = "indexes/" + indexId + "/view?";
  var limit = $('#index-limit').val() * 1;
  
  // Initialize some values if we're at the beginning of the listing
  if (!prevkeys) {
    var supplied_val = $('#index-filter').val();
    var json_encoded = JSON.stringify(supplied_val);
    startkey = btoa(unescape(encodeURIComponent(json_encoded)));
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
  
  $.get(url, function(data) {
          $('#index-list-view').html(data);
    
          $('#previous-page').button(
            {
              icons: {primary:'ui-icon-circle-arrow-w'} 
            }).click(function() 
                     {
                       getIndexView(prevkeys.pop(), 
                                    previds.pop(), 
                                    prevkeys, 
                                    previds);
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
                       prevkeys.push(prevkey);
                       previds.push(previd);
                       
                       getIndexView(nextkey, nextid, prevkeys, previds);
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

};

$(function () {
    $('#index-builder-dialog').hide();
    $('#index-new-dialog').hide();
    initIndexEditButtons(function () {return $('#index-editing-data');});
    initIndexNewButton();
    $('#button-bar').buttonset();
    initIndexIndex();
    $('#index-filter-form input').keyup(function() {
                                          getIndexView();
                                        });
  });
