// Dialog for manipulating fieldsets

function initFieldsetAddDialog() {
  var dialog = $("#fieldset-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Create": function() {
        var fieldsetName = $("#fieldset-name-input");
        var fieldsetLabel = $("#fieldset-label-input");
        var fieldsetDescription = $("#fieldset-description-input");
        var fieldsetOrder = $("#fieldset-order-input");
        var fieldsetDoctype = $("#fieldset-doctype-input");
        var fieldsetMultiple = $("#fieldset-multiple-input");
        var fieldsetCollapse = $("#fieldset-collapse-input");
        
        $('.input').removeClass('ui-state-error');
        
        checkResult = true;
        
        if (checkResult) {
          var obj = {
            "category": "fieldset", 
            "name": fieldsetName.val(),
            "label": fieldsetLabel.val(),
            "description": fieldsetDescription.val(),
            "order": (fieldsetOrder.val() * 1),
            "doctype": fieldsetDoctype.val(),
            "multiple": fieldsetMultiple.is(':checked'),
            "collapse": fieldsetCollapse.is(':checked')
          },
          complete = function(context) {
            populateFieldsets(fieldsetDoctype.val());
            $(context).dialog("close");
          };
          
          sendConfigDoc("config/doctypes/" + fieldsetDoctype.val() + "/fieldsets", obj, 'POST', complete, this);
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

function initFieldsetEditDialog(id, doctype, oldobj, rev) {
  var fieldsetName = $("#fieldset-name-input");
  var fieldsetLabel = $("#fieldset-label-input");
  var fieldsetDescription = $("#fieldset-description-input");
  var fieldsetOrder = $("#fieldset-order-input");
  var fieldsetMultiple = $("#fieldset-multiple-input");
  var fieldsetCollapse = $("#fieldset-collapse-input");
  var url = "config/doctypes/" + doctype + 
            "/fieldsets/" + id + "?rev=" + rev;
            
  fieldsetName.val(oldobj.name);
  fieldsetLabel.val(oldobj.label);
  fieldsetDescription.val(oldobj.description);
  fieldsetOrder.val(oldobj.order);
  if (oldobj.multiple == "true") {
    fieldsetMultiple.attr('checked', true);
  }
  if (oldobj.collapse == "true") {
    fieldsetCollapse.attr('checked', true);
  }
        
  var dialog = $("#fieldset-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function() {
        
        $('.input').removeClass('ui-state-error');
        
        checkResult = true;
        
        if (checkResult) {
          var obj = {
            "_id": id, 
            "category": "fieldset", 
            "name": fieldsetName.val(),
            "label": fieldsetLabel.val(),
            "description": fieldsetDescription.val(),
            "order": (fieldsetOrder.val() * 1),
            "doctype": doctype,
            "multiple": fieldsetMultiple.is(':checked'),
            "collapse": fieldsetCollapse.is(':checked')
          },
          complete = function(context) {
            populateFieldsets(doctype);
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

