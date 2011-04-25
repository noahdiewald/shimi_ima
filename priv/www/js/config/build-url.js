// Build a url based on information available on page

function buildUrl(source, prefix) {
  var url = {};
  
  if (prefix) {
    prefix = prefix + "-";
  } else {
    prefix = "";
  }
  
  url.string = "config/";
  url.origin = source;
  url.type = prefix + "url";
  url.valid_components = ["doctype", "fieldset", "field"];
  
  url.send = function(object, method, callback, context) {
    sendConfigDoc(url.toString(), object, method, callback, context);
    return url;
  };
  
  url.put = function(object, callback, context) {
    url.send(object, 'PUT', callback, context);
    return url;
  };
    
  url.post = function(object, callback, context) {
    url.send(object, 'POST', callback, context);
    return url;
  };
  
  url.delete = function(callback, context) {
    url.send({}, 'DELETE', callback, context);
    return url;
  };
    
  url.toString = function() {
    var rev;
      
    urlString = url.string.concat(url.valid_components.map(function(item) {
      var plural = item + "s";
      var value = url[item];
        
      if (value) {
        return plural + "/" + value;
      } else {
        return plural;
      }
    }).join("/"));
      
    if (rev = getData(prefix + 'rev', source)) {
      urlString = urlString.concat("?rev=" + rev);
    }
      
    return urlString;
  };
  
  url.valid_components.forEach(function(item) {
    url[item] = (function() {
      return getData(prefix + item, source)
    })();
  });

  return url; 
}
