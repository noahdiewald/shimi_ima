// Add events to a fieldset. In this case a change event that will populate
// the select options of a following field input element.  queryDoctype may
// be either a string corresponding to the doctype or an input field with
// the doctype as a value. The queryFieldset and queryField should both
// be select elements.

function setQueryFieldsetEvents(queryDoctype, queryFieldset, queryField) {
  queryFieldset.change(function() {
    if (!(typeof queryDoctype == "string")) {
      queryDoctype = queryDoctype.val();
    }
    
    var url = 'doctypes/' + queryDoctype + 
              '/fieldsets/' + queryFieldset.val() + '/fields?as=options';
    
    fillOptionsFromUrl(url, queryField);
  });
  
  return false;
}

function setQueryFieldEvents(queryDoctype, queryFieldset, queryField) {
  queryField.change(function() {
    var fieldId = queryField.val();
    var fieldsetId = queryFieldset.val();
    
    if (!(fieldId.isBlank())) {
      getFieldDoc(fieldId, fieldsetId, queryDoctype, function(data) {
        alterOperatorField(data, fieldId);
      });
    }
  });
  
  return false;
}

function setQueryOperatorEvents(argumentField, operatorField, fieldField) {
  operatorField.change(function() {
    alterArgumentField(argumentField, operatorField, fieldField);
  });
  
  return false;
}
