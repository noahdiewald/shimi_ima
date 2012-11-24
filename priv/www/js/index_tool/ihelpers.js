shimi.ihelpers = function() {
  var mod = {};
  var s = shimi.sess();

  mod.getFieldDoc = function(fieldId, fieldsetId, doctypeId, callback) {
    var fieldDoc = s.get(fieldId);
    var url = 'doctypes/' + doctypeId + 
      '/fieldsets/' + fieldsetId + 
      '/fields/' + fieldId + '?format=json';
              
    if (fieldDoc) {
      if (callback) {
        callback(fieldDoc);
      }
      return fieldDoc;
    } else {
      $.ajax({
               url: url,
               async: false,
               dataType: 'json',
               success: function(data) {
                 s.put(data);
                 if (callback) {
                   callback(s.get(fieldId));
                 }
               }
             });
            
      return s.get(fieldId);
    }
  };
  
  return mod;
};