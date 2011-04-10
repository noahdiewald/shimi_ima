function putDoc(doc) {
  if (!sessionStorage[doc._id]) {
    sessionStorage[doc._id] = JSON.stringify(doc);
  }
  
  return doc._id;
}

function getDoc(docId) {
  var doc = sessionStorage[docId];
  
  if (doc) {
    return JSON.parse(doc);
  } else {
    return null;
  }
}

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

function setQueryFieldsetEvents(queryDoctype, queryFieldset, queryField) {
  queryFieldset.change(function() {
    if (!(typeof queryDoctype == "string")) {
      queryDoctype = queryDoctype.val();
    }
    
    var url = 'doctypes/' + queryDoctype + 
              '/fieldsets/' + queryFieldset.val() + '/fields?as=options';
    
    fillOptionsFromUrl(url, queryField);
  });
  
  return false;
}

function setQueryFieldEvents(queryDoctype, queryFieldset, queryField) {
  queryField.change(function() {
    if (!(queryField.val().isBlank())) {
      fieldDoc = getFieldDoc(queryField.val(), queryFieldset.val(), queryDoctype);
      alterOperatorField(fieldDoc, queryField.val());
    }
  });
  
  return false;
}

function getFieldDoc(fieldId, fieldsetId, doctypeId) {
  var fieldDoc;
  var url = 'doctypes/' + doctypeId + 
            '/fieldsets/' + fieldsetId + 
            '/fields/' + fieldId + '?format=json';
            
  if (fieldDoc = getDoc(fieldId)) {
    return fieldDoc;
  } else {
    $.getJSON(url, function(data) {
      putDoc(data);
    });
    
    return getDoc(fieldId);
  }
}

function setQueryOperatorEvents(argumentField, operatorField, fieldField) {
  operatorField.change(function() {
    alterArgumentField(argumentField, operatorField, fieldField);
  });
  
  return false;
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
      argumentField.datepicker({dateFormat: "yy-mm-dd"});
    } else {
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

function initQueryNewDialog() {
  var queryDoctype = $("#query-doctype-input");
  var queryFieldset = $("#query-fieldset-input");
  var queryField = $("#query-field-input");
  var queryName = $("#query-name-input");
  
  var dialog = $("#query-new-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Create": function() {
        $('.input').removeClass('ui-state-error');
        
        // place holder for client side validation
        var checkResult = true;
        
        if (checkResult) {
          var obj = {
            "category": "query", 
            "name": queryName.val(), 
            "conditions": [], 
            "doctype": queryDoctype.val(),
            "fieldset": queryFieldset.val(),
            "field": queryField.val()
          },
          complete = function(context) {
            initQueryIndex();
            $(context).dialog("close");
          };
          sendConfigDoc("queries", obj, 'POST', complete, this);
        }
      },
      "Cancel": function() {
        $(this).dialog("close");
      }
    },
    close: function() {
      queryFieldset.unbind('change');
      queryDoctype.unbind('change');
      clearValues($('.input')).removeClass('ui-state-error');
    }
  });
  
  setQueryDoctypeEvents(queryDoctype, queryFieldset);
  setQueryFieldsetEvents(queryDoctype, queryFieldset, queryField);
  
  return dialog;
}

function initConditionRemoveButtons(tableBody) {
  tableBody.find('button').button({
    icons: {primary: "ui-icon-minus"}
  }).click(function(e) {
    $(e.target).parent('td').parent('tr').remove();
  });
  
  return false;
}

function initQueryBuilderDialog(queryDoctype) {
  var builderOr = $("#builder-or-input");
  var builderNegate = $("#builder-negate-input");
  var builderOperator = $("#builder-operator-input");
  var builderArgument = $("#builder-argument-input");
  var builderFieldset = $("#builder-fieldset-input");
  var builderField = $("#builder-field-input");
  var notBlank = [builderOperator, builderFieldset, builderField];
  var fieldset_url = 'doctypes/' + queryDoctype + '/fieldsets';
  var condition_url = 'queries/condition';
  
  var appendCondition = function(builderRow) {
    tableBody = $('#query-conditions-listing tbody');
    tableBody.append(builderRow);
    tableBody.sortable();
    initConditionRemoveButtons(tableBody);
    
    return false;
  };
    
  fillOptionsFromUrl(fieldset_url, builderFieldset);
  
  builderOr.change(function() {
    if (builderOr.is(':checked')) {
      $('#builder-conditions').hide();
    } else {
      $('#builder-conditions').show();
    }
  });
  
  var dialog = $("#query-builder-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Create": function() {
        $('.input').removeClass('ui-state-error');
        
        // place holder for client side validation
        var checkResult = true;
        
        if (!builderOr.is(':checked')) {
          notBlank.forEach(function(item) {
            if (item.val().isBlank()) {
              item.addClass('ui-state-error');
              checkResult = false;
            } else {
              item.removeClass('ui-state-error');
            }
          });
        }
        
        if (checkResult) {
          if (builderOr.is(':checked')) {
            $.get(condition_url, {"is_or": true}, function(data) {appendCondition(data)});
          } else {
            $.get(condition_url, {
              "is_or": false,
              "negate": builderNegate.is(':checked'),
              "fieldset": builderFieldset.val(),
              "field": builderField.val(),
              "operator": builderOperator.val(),
              "argument": builderArgument.val()
            }, function(data) {appendCondition(data)});
          }
          
          $(this).dialog("close");
        }
      },
      "Cancel": function() {
        $(this).dialog("close");
      }
    },
    close: function() {
      $('#builder-conditions').show();
      builderFieldset.unbind('change');
      builderField.unbind('change');
      builderOperator.unbind('change');
      clearValues($('.input')).removeClass('ui-state-error');
    }
  });
  
  setQueryFieldsetEvents(queryDoctype, builderFieldset, builderField);
  setQueryFieldEvents(queryDoctype, builderFieldset, builderField);
  setQueryOperatorEvents(builderArgument, builderOperator, builderField);
  
  return dialog;
}

function initQueryNewButton() {
  $('#new-query-button').button({
    icons: {primary: "ui-icon-plus"}
  }).click(function() {
    initQueryNewDialog().dialog("open");
  });
  
  return false;
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
    
    argument = fixArgumentType(argument, fieldDoc.subcategory);
    
    var condition = {
      "is_or": row.find('td.or-condition').attr('data-value') == "true",
      "negate": row.find('td.negate-condition').attr('data-value') == "true",
      "fieldset": fieldsetId,
      "field": fieldId,
      "operator": row.find('td.operator-condition').attr('data-value'),
      "argument": argument
    };
    
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
    var completeFunction = function() {
      getQueryEdit(buttonData.attr('data-query-id'));
      flashHighlight("Success", "Your query has been saved.");
    };
    
    saveQuery(buttonData, completeFunction);
  });
}

function initQueryDeleteButton(button, buttonData) {
  button.button({icons: {primary: "ui-icon-trash"}}).click(function (e) {
    var deleteButton = $(e.target);
    var queryId = buttonData.attr('data-query-id');
    var queryRev = buttonData.attr('data-query-rev');
    var completeMessage = "Your query has been deleted.";
    var completeFunction = function() {
      button.parent('div').parent('div').empty();
      initQueryIndex();
    };
    
    if (confirm("Are you sure?")) {
      deleteQuery(queryId, queryRev, completeMessage, completeFunction);
    }
  });
  
  return false;
}

function initQueryAddConditionButton(button, buttonData) {
  button.button({
    icons: {primary: "ui-icon-plus"}
  }).click(function (e) {
    initQueryBuilderDialog(buttonData.attr('data-query-doctype')).dialog("open");
  });

  return false;
}

function getQueryEdit(queryId) {
  var url = "queries/" + queryId;
  var target = $('#query-edit');
  
  $.get(url, function(queryData) {
    target.html(queryData);
    initQueryEditButtons($('#query-editing-data'));
    // TODO don't repeat this code. It is also in initQueryBuilderDialog
    tableBody = $('#query-conditions-listing tbody');
    tableBody.sortable();
    initConditionRemoveButtons(tableBody);
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
    });
  });
}

$(function () {
  initTabs(); 
  $('#query-builder-dialog').hide();
  $('#query-new-dialog').hide();
  initQueryNewButton();
  initQueryIndex();
});
