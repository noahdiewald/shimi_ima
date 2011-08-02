// Add events to a fieldset. In this case a change event that will populate
// the select options of a following field input element.  queryDoctype may
// be either a string corresponding to the doctype or an input field with
// the doctype as a value. The queryFieldset and queryField should both
// be select elements.

function setQueryDoctypeEvents(queryDoctype, queryFieldset, callback) {
  queryDoctype.change(function() {
    var url = 'doctypes/' + queryDoctype.val() + '/fieldsets';
    var callback2;
    
    if (callback) callback2 = callback();
    
    fillOptionsFromUrl(url, queryFieldset, callback2);
  });
  
  return false;
}

function setQueryFieldsetEvents(queryDoctype, queryFieldset, queryField, callback) {
  queryFieldset.change(function() {
    if (!(typeof queryDoctype == "string")) {
      queryDoctype = queryDoctype.val();
    }
    
    var url = 'doctypes/' + queryDoctype + 
              '/fieldsets/' + queryFieldset.val() + '/fields?as=options';
    
    if (callback) callback2 = callback();
    
    fillOptionsFromUrl(url, queryField, callback2);
  });
  
  return false;
}

function setQueryFieldEvents(queryDoctype, queryFieldset, queryField, callback) {
  queryField.change(function() {
    var fieldId = queryField.val();
    var fieldsetId = queryFieldset.val();
    var callback2;
    
    if (callback) callback2 = callback();
    
    if (!(fieldId.isBlank())) {
      getFieldDoc(fieldId, fieldsetId, queryDoctype, function(data) {
        alterOperatorField(data, fieldId, callback2);
      });
    }
  });
  
  return false;
}

function setQueryOperatorEvents(argumentField, operatorField, fieldField, callback) {
  operatorField.change(function() {
  
    if (callback) callback2 = callback();
    
    alterArgumentField(argumentField, operatorField, fieldField, callback2);
  });
  
  return false;
}
