function(doc) { 
        if (doc._attachments) {
          if (typeof Object.keys !== 'function') {
            Object.keys = function(obj) {
              if (typeof obj !== 'object' && typeof obj !== 'function' || obj === null) {
                throw new TypeError('Object.keys called on non-object');
              }
              var keys = [];
              for (var p in obj) {
                obj.hasOwnProperty(p) && keys.push(p);
              }
              return keys;
            }
          }
        
          var path = [];
          var filename = Object.keys(doc._attachments)[0];
          var attachment = doc._attachments[filename];
          var content_type = attachment.content_type;
          var size = attachment.length / (1024 * 1024);
          
          if (doc.path)  {
            path = doc.path;
          }
        
          emit(content_type, {filename: filename, path: path, size: size.toFixed(3), rev: doc._rev}) ;
        }
      }