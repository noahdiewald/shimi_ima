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
    var url = 'doctypes/' + queryDoctype.val() + 
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
  var allFields = [queryDoctype, queryFieldset, queryField, queryName];
  
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

function initQueryNewButton() {
  $('#new-query-button').button({
    icons: {primary: "ui-icon-plus"}
  }).click(function() {
    initQueryNewDialog().dialog("open");
  });
  
  return false;
}

function getQueryEdit(queryId) {
  var url = "queries/" + queryId;
  var target = $('#query-edit');
  
  $.get(url, function(queryData) {
    target.html(queryData);
    $('#all-query-container').accordion("activate", 1);
  });
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
  
  //getIndex();
  //initEdit();
  
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
