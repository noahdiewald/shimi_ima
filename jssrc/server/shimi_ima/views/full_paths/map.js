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
          
          if (doc['path'])  {
            path = path.concat(doc['path']);
          }
          
          path.push(filename);
          
          emit(path);
        }
      }