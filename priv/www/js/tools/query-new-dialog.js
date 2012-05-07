function initQueryNewDialog() {
  var queryDoctype = $("#query-doctype-input");
  var queryFieldset = $("#query-fieldset-input").inputDisable();
  var queryField = $("#query-field-input").inputDisable();
  var queryName = $("#query-name-input");
  
  var doctypeEvents = function() {
    setQueryDoctypeEvents(queryDoctype, queryFieldset, function() {
                            queryFieldset.inputDisable();
                            queryField.inputDisable();
                            
                            return function() {
                              queryFieldset.inputEnable();
                            };
                          });
  };
  
  var fieldsetEvents = function() {
    setQueryFieldsetEvents(queryDoctype, queryFieldset, queryField, function() {
      queryField.inputDisable();
      
      return function() {
        queryField.inputEnable();
      };
    });
  };
  
  var getLabelForVal = function(val) {
    return $('option[value="'+ val + '"]').text();
  };

  var getLabel = function() {
    return [getLabelForVal(queryFieldset.val()), 
            getLabelForVal(queryField.val())].join(":");
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
            "show_deleted": false,
            "conditions": [], 
            "doctype": queryDoctype.val(),
            "fields_label": [getLabel()],
            "fields": [queryField.val()]
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
  
