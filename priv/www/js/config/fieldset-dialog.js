function fieldsetDialog(url, values) {
  var f = fieldsetElems().get(values);
  
  var dialog = $("#fieldset-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function() {
        var obj = f.getFieldsetInputVals();
        var complete = function(context) {
          populateFieldsets(url);
          $(context).dialog("close");
        }
        if (!values.rev || values.rev.isBlank()) {
          url.post(obj, complete, this);
        } else {
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
