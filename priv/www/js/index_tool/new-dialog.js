function initIndexNewDialog() {
  var indexDoctype = $("#index-doctype-input");
  var indexFieldset = $("#index-fieldset-input").inputDisable();
  var indexField = $("#index-field-input").inputDisable();
  var indexName = $("#index-name-input");
  
  var doctypeEvents = function() {
    setIndexDoctypeEvents(indexDoctype, indexFieldset, function() {
                            indexFieldset.inputDisable();
                            indexField.inputDisable();
                            
                            return function() {
                              indexFieldset.inputEnable();
                            };
                          });
  };
  
  var fieldsetEvents = function() {
    setIndexFieldsetEvents(indexDoctype, indexFieldset, indexField, function() {
                             indexField.inputDisable();
      
                             return function() {
                               indexField.inputEnable();
                             };
                           });
  };
  
  var getLabelForVal = function(val) {
    return $('option[value="'+ val + '"]').text();
  };

  var getLabel = function() {
    return [getLabelForVal(indexFieldset.val()), 
            getLabelForVal(indexField.val())].join(":");
  };

  var dialog = $("#index-new-dialog")
    .dialog({
              autoOpen: false,
              modal: true,
              buttons: {
                "Create": function() {
                  $('.input').removeClass('ui-state-error');
        
                  // place holder for client side validation
                  var checkResult = true;
        
                  if (checkResult) {
                    var obj = {
                      "category": "index", 
                      "name": indexName.val(),
                      "show_deleted": false,
                      "conditions": [], 
                      "doctype": indexDoctype.val(),
                      "fields_label": [getLabel()],
                      "fields": [indexField.val()]
                    },
                    complete = function(context) {
                      initIndexIndex();
                      $(context).dialog("close");
                    };
                    sendConfigDoc("indexes", obj, 'POST', complete, this);
                  }
                },
                "Cancel": function() {
                  $(this).dialog("close");
                }
              },
              close: function() {
                indexFieldset.unbind('change');
                indexDoctype.unbind('change');
                clearValues($('.input')).removeClass('ui-state-error');
              }
            });
  
  doctypeEvents();
  fieldsetEvents();
  
  return dialog;
}
