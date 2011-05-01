// Dialog for manipulating doctypes

function doctypeDialog(url, values) {
  var f = doctypeElems().get(values);
  
  if (values.rev && !values.rev.isBlank()) {
    f.doctype.attr('disabled', 'disabled');
  }  
  
  var dialog = $("#doctype-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function() {
        var obj = f.getDoctypeInputVals();
        var complete = function(context) {
          populateDoctypeTabs();
          $(context).dialog("close");
        };
        
        if (!values.rev || values.rev.isBlank()) {
          url.post(obj, complete, this);
        } else {
          obj._id = url.doctype;
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
