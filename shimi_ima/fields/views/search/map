function(doc) { 
             if (!doc.deleted_ && doc.index) { 
               var isArray = function(anArray) {
                 return Object.prototype.toString.apply(anArray) === 
                   '[object Array]';
               };
               var notBlank = function(val) {
                 return (val !== undefined && val !== null && val.toString() !== '');
               };
               var head = function() {
                 hd = doc.head;
                 return hd.map(function(h) {
                   var v = doc.index[h];
                   if (isArray(v[0])) {
                     if (notBlank(v[0][1])) {
                       return v[0][1] + '...';
                     } else {
                       return '...';
                     }
                   } else {
                     return v[1];
                   }
                 }).join(', ');
               };
               var condEmit = function(key, val) {
                 if (notBlank(val[1])) {
                   emit([doc.doctype, key, val[0], val[1].toString()], head());
                 }
               };
               
               Object.keys(doc.index).forEach(function(indexKey) {
                 if (!isArray(doc.index[indexKey][0])) {
                   condEmit(indexKey, doc.index[indexKey]);
                 } else {
                   doc.index[indexKey].forEach(function(val) {
                     condEmit(indexKey, val);
                   });
                 }
               });
             }
           }