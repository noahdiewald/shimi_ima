function(doc) { 
        if (doc._attachments) {
          if (typeof Object.keys != 'function') {
            Object.keys = function(obj) {
              if (typeof obj != 'object' && typeof obj != 'function' || obj == null) {
                throw TypeError('Object.keys called on non-object');
              }
              var keys = [];
              for (var p in obj) obj.hasOwnProperty(p) &&keys.push(p);
              return keys;
            }
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