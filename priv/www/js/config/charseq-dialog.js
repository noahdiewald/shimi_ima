// Dialog for manipulating doctypes

function charseqDialog(values) {
  var f = charseqElems().get(values);
  
  var dialog = $("#charseq-dialog").dialog({
    autoOpen: false,
    modal: true,
    buttons: {
      "Save": function() {
        var obj = f.getCharseqInputVals();
        var url = 'config/charseqs';
        var method = 'POST';
        var complete = function(context) {
          populateCharseqTabs();
          $(context).dialog("close");
        };
        
        if (values.rev && (!values.rev.isBlank())) {
          method = 'PUT';
          url = 'config/charseqs/' + obj._id + '?rev=' + obj.rev;
        }
        
        sendConfigDoc(url, obj, method, complete, this);
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
