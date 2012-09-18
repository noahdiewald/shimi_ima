// Returns an object with references to add/edit doctype dialog
// field elements with helper functions. 

function doctypeElems() {
  var fElems = {};
  
  fElems.attrs = ["description", "doctype", "rev"];
               
  fElems.get = function(values) {
    var fObj = {};
    
    fObj.attrs = fElems.attrs;
    
    fObj.copyValues = function(source) {
      Object.keys(source).forEach(function(field) {
        fObj[field].val(source[field]);
      });
      return fObj;
    };
    
    fObj.getDoctypeInputVals = function() {
      var valObj = {
        "category": "doctype",
        "description": fObj.description.val(),
        "_id": fObj.doctype.val()
      };
      return valObj;
    };
    
    fObj.clear = function() {
      clearValues($('#doctype-dialog .input')).removeClass('ui-state-error');
      return fObj;
    };
                   
    fObj.attrs.forEach(function(item) {
      fObj[item] = $('#doctype-' + item + '-input');
    });
    
    fObj.copyValues(values);
      
    return fObj;
  };
  
  return fElems;
}

