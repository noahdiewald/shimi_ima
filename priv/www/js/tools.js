function initTabs() {
  $("#main-tabs").tabs();
  //populateDoctypeTabs();
  
  return false;
}

function initHelpText() {
  $("#doctype-info").hide();
  $("#charseq-info").hide();

  $("#doctype-info-toggle").click(function() {
    $("#doctype-info").toggle("blind", {}, 500);
    return false;
  });
  
  $("#charseq-info-toggle").click(function() {
    $("#charseq-info").toggle("blind", {}, 500);
    return false;
  });
  
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

function fillOptionsFromUrl(url, selectElement) {
  $.get(url, function(options) {
    selectElement.html(options);
  });
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
  var notBlank = [builderOperator, builderArgument, builderFieldset, builderField];
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
      clearValues($('.input')).removeClass('ui-state-error');
    }
  });
  
  setQueryFieldsetEvents(queryDoctype, builderFieldset, builderField);
  
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

function getQueryConditions(rows) {
  conditons = rows.map(function(row) {
    var condition = {
      "is_or": row.find('td.or-condition').attr('data-value') == "true",
      "negate": row.find('td.negate-condition').attr('data-value') == "true",
      "fieldset": row.find('td.fieldset-condition').attr('data-value'),
      "field": row.find('td.field-condition').attr('data-value'),
      "operator": row.find('td.operator-condition').attr('data-value'),
      "argument": row.find('td.argument-condition').attr('data-value')
    };
    
    return condition;
  });
  
  return conditions;
}

function saveQuery(buttonData, completeFunction) {
  var queryId = buttonData.attr('data-query-id');
  var queryRev = buttonData.attr('data-query-rev');
  var url = "queries/" + queryId + "?rev=" + queryRev;
  
  var obj = {
    "_id": queryId,
    "doctype": buttonData.attr('data-query-doctype'),
    "fieldset": buttonData.attr('data-query-fieldset'),
    "field": buttonData.attr('data-query-field'),
    "name": buttonData.attr('data-query-name'),
    "conditions": getQueryConditions($('#query-conditions-listing tbody tr'))
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
    var completeMessage = "Your query has been saved.";
    var completeFunction = function() {};
    
    saveQuery(buttonData, completeMessage, completeFunction);
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
      $('#all-query-container').accordion("activate", 0);
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
    $('#all-query-container').accordion("activate", 1);
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
  initHelpText();
  $('#query-builder-dialog').hide();
  $('#query-new-dialog').hide();
  initQueryNewButton();
  initQueryIndex();
  
  //$('#index-filter-form input').keyup(function() {
  //  getIndex();
  //});
  
  //$('#index-filter-form select').change(function() {
  //  getIndex();
  //});
  
  $('#all-query-container').accordion({ 
    collapsible: true,
    autoHeight: false
  });
});
