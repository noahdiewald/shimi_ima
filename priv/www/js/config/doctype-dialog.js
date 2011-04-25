// Dialog for manipulating doctypes

function initDoctypeAddDialog() {
  var doctypeName = $("#doctype-name-input");
  doctypeName.attr('disabled', false);
  
  var dialog = $("#doctype-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Add Document Type": function() {
        $('.input').removeClass('ui-state-error');
        
        checkResult = true;
        
        if (checkResult) {
          var obj = {
            "category": "doctype", 
            "description": $("#doctype-description-input").val(),
            "_id": $("#doctype-name-input").val()
          },
          complete = function(context) {
            populateDoctypeTabs();
            $(context).dialog("close");
          };
          sendConfigDoc("config/doctypes", obj, 'POST', complete, this);
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

function initDoctypeEditDialog(name, description, rev) {
  var doctypeName = $("#doctype-name-input");
  var doctypeDescription = $("#doctype-description-input");
  var url = "config/doctypes/" + name + "?rev=" + rev;
  
  doctypeDescription.val(description);
  doctypeName.val(name);
  doctypeName.attr('disabled', true);

  var dialog = $("#doctype-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function() {
        $('.input').removeClass('ui-state-error');
        
        checkResult = true;
        
        if (checkResult) {
          var obj = {
            "category": "doctype", 
            "description": $("#doctype-description-input").val(),
            "_id": $("#doctype-name-input").val()
          },
          complete = function(context) {
            populateDoctypeTabs();
            $(context).dialog("close");
          };
          
          sendConfigDoc(url, obj, 'PUT', complete, this);
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
