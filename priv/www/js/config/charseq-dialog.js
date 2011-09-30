// Dialog for manipulating doctypes

function charseqDialog(url, values) {
  var f = charseqElems().get(values);
  
  var dialog = $("#charseq-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function() {
        var obj = f.getCharseqInputVals();
        var complete = function(context) {
          populateCharseqTabs();
          $(context).dialog("close");
        };
        
        if (!values.rev || values.rev.isBlank()) {
          url.post(obj, complete, this);
        } else {
          obj._id = url.charseq;
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
