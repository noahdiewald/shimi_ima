function(doc) { 
        if (doc._attachments) {
          if (typeof Object.keys !== 'function') {
            Object.keys = function(o){
              if (o !== Object(o)) {
                throw new TypeError('Object.keys called on non-object');
              }
              var ret=[],p;
              for(p in o) {
                if (Object.prototype.hasOwnProperty.call(o,p)) {
                  ret.push(p);
                }
              }
              return ret;
            };
          }
        
          var path = [];
          var filename = Object.keys(doc._attachments)[0];
          var attachment = doc._attachments[filename];
          var content_type = attachment.content_type;
          var size = attachment.length / (1024 * 1024);
          
          if (doc.path)  {
            path = doc.path;
          }
        
          emit(filename, {content_type: content_type, path: path, size: size.toFixed(3), rev: doc._rev}) ;
        }
      }