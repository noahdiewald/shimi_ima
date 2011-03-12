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

function initQueryNewDialog() {
  var queryOwner = $("#query-owner-input");
  var queryDoctype = $("#query-doctype-input");
  var queryName = $("#query-name-input");
  
  var dialog = $("#query-new-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Create": function() {
        $('.input').removeClass('ui-state-error');
        
        // place holder for client side validation
        checkResult = true;
        
        if (checkResult) {
          var obj = {
            "category": "query", 
            "owner": queryOwner.val(), 
            "name": queryName.val(), 
            "doctype": queryDoctype.val()
          },
          complete = function(context) {
            //populateDoctypeTabs();
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

$(function () {
  initTabs(); 
  initHelpText();
  $('#query-builder-dialog').hide();
  $('#query-new-dialog').hide();
  initQueryNewButton();
  
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
