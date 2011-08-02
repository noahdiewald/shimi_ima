function initQueryNewDialog() {
  var queryDoctype = $("#query-doctype-input");
  var queryFieldset = $("#query-fieldset-input");
  var queryField = $("#query-field-input");
  var queryName = $("#query-name-input");
  
  var doctypeEvents = function() {
    setQueryDoctypeEvents(queryDoctype, queryFieldset, function() {
      queryFieldset.val('').attr('disabled', 'disabled');
      queryField.val('').attr('disabled', 'disabled');
      
      return function() {
        queryFieldset.removeAttr('disabled');
      };
    });
  };
  
  var fieldsetEvents = function() {
    setQueryFieldsetEvents(queryDoctype, queryFieldset, queryField, function() {
      queryField.val('').attr('disabled', 'disabled');
      
      return function() {
        queryField.removeAttr('disabled');
      };
    });
  };
  
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
      queryFieldset.unbind('change');
      queryDoctype.unbind('change');
      clearValues($('.input')).removeClass('ui-state-error');
    }
  });
  
  doctypeEvents();
  fieldsetEvents();
  
  return dialog;
}
  
