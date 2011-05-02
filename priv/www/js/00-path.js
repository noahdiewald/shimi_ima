// Build a url based on information available on page

function path(source, category, section) {
  var path = {};
  
  if (category) {
    var prefix = category + "-";
  } else {
    var prefix = "";
  }
  
  if (section) {
    path.string = section + "/";
  } else {
    path.string = "";
  }
  
  path.category = category;
  path.origin = source;
  path.type = prefix + "path";
  path.valid_components = ["doctype", "fieldset", "field"];
  
  path.valid_components.forEach(function(item) {
    path[item] = (function() {
      var value = getData(prefix + item, path.origin);
      return value;
    })();
  });
  
  path.rev = getData(prefix + 'rev', path.origin);
  
  path.send = function(object, method, callback, context) {
    sendConfigDoc(path.toString(), object, method, callback, context);
    return path;
  };
  
  path.put = function(object, callback, context) {
    path.send(object, 'PUT', callback, context);
    return path;
  };
    
  path.post = function(object, callback, context) {
    path.send(object, 'POST', callback, context);
    return path;
  };
  
  path.delete = function(callback, context) {
    path.send({}, 'DELETE', callback, context);
    return path;
  };
    
  path.toString = function() {
    var rev;
      
    pathString = path.string.concat(path.valid_components.map(function(item) {
      var plural = item + "s";
      var value = path[item];
      
      if (value) {
        return plural + "/" + value;
      } else if (item == path.category) {
        return plural;
      }
    }).filter(function(item) {
      return (typeof item == "string" && !item.isBlank());
    }).join("/"));
      
    if (path.rev) {
      pathString = pathString.concat("?rev=" + path.rev);
    }
    
    return pathString;
  };
  
  return path; 
}
