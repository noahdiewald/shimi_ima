// Add events to a fieldset. In this case a change event that will populate
// the select options of a following field input element.  indexDoctype may
// be either a string corresponding to the doctype or an input field with
// the doctype as a value. The indexFieldset and indexField should both
// be select elements.

function setIndexDoctypeEvents(indexDoctype, indexFieldset, callback) {
  indexDoctype
    .change(function() {
              var url = 'doctypes/' + 
                indexDoctype.val() + '/fieldsets';
              var callback2;
              
              if (callback) callback2 = callback();
              
              fillOptionsFromUrl(url, indexFieldset, callback2);
            });
  
  return false;
}

function setIndexFieldsetEvents(indexDoctype, indexFieldset, indexField, 
                                callback) {
  indexFieldset
    .change(function() {
              var callback2;

              if (!(typeof indexDoctype == "string")) {
                indexDoctype = indexDoctype.val();
              }
              
              if (indexFieldset.val()) {
                var url = 'doctypes/' + indexDoctype + 
                  '/fieldsets/' + indexFieldset.val() + '/fields?as=options';
    
                if (callback) callback2 = callback();
    
                fillOptionsFromUrl(url, indexField, callback2);
              }
            });
  
  return false;
}

function setIndexFieldEvents(indexDoctype, indexFieldset, indexField, 
                             callback) {
  indexField
    .change(function() {
              var fieldId = indexField.val();
              var fieldsetId = indexFieldset.val();
              var callback2;
    
              if (callback) callback2 = callback();
    
              if (!(fieldId.isBlank())) {
                getFieldDoc(fieldId, fieldsetId, indexDoctype, function(data) {
                              alterOperatorField(data, fieldId, callback2);
                            });
              }
            });
  
  return false;
}

function setIndexOperatorEvents(argumentField, operatorField, fieldField, 
                                callback) {
  operatorField
    .change(function() {
              var callback2;
              
              if (callback) callback2 = callback();
              
              alterArgumentField(argumentField, operatorField, fieldField, 
                                 callback2);
            });
  
  return false;
}
