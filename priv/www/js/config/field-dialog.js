// Dialog for manipulating fields

function fieldDialog(url, values) {
  var f = fieldElems().get(values);
   
  var dialog = $("#field-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function() {
        var obj = f.getFieldInputVals();
        var complete = function(context) {
          populateFields(url);
          $(context).dialog("close");
        };
        if (!values.rev || values.rev.isBlank()) {
          url.post(obj, complete, this);
        } else {
          obj._id = url.field;
          url.put(obj, complete, this);
        }
      },
      "Cancel": function() {
        $(this).dialog("close");
      }
    },
    close: function() {
      f.clear();
    }
  });
  
  return dialog;
}
