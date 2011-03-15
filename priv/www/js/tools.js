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
  setQueryFieldsetEvents(queryDoctype.val(), queryFieldset, queryField);
  
  return dialog;
}

function initQueryBuilderDialog(queryDoctype) {
  var builderOr = $("#builder-or-input");
  var builderNegate = $("#builder-negate-input");
  var builderOperator = $("#builder-operator-input");
  var builderValue = $("#builder-value-input");
  var builderFieldset = $("#builder-fieldset-input");
  var builderField = $("#builder-field-input");
  var notBlank = [builderOperator, builderValue, builderFieldset, builderField];
  var url = 'doctypes/' + queryDoctype + '/fieldsets';
    
  fillOptionsFromUrl(url, builderFieldset);
  
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
            // TODO do this through back end templates
            builderRow = '<tr><td colspan=5 class="or-conditon" data-value="or">OR</td></tr>';
          } else {
            builderRow = '\n\
            <tr>\
              <td data-name="negate" data-value="' + builderNegate.is(':checked') + '">' + builderNegate.is(':checked') + '</td>\
              <td data-name="fieldset" data-value="' + builderFieldset.val() + '">' + builderFieldset.val() + '</td>\
              <td data-name="field" data-value="' + builderField.val() + '">' + builderField.val() + '</td>\
              <td data-name="condition" data-value="' + builderOperator.val() + '">' + builderOperator.val() + '</td>\
              <td data-name="value" data-value="' + builderValue.val() + '">' + builderValue.val() + '</td>\
            </tr>'
          }
          
          $('#query-conditions-listing tbody').append(builderRow);
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
  button.button({icons: {primary: "ui-icon-file"}});
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
