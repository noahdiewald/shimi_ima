/*
 h1. Path helper
 
 h2. Creating the object
 
 This function returns an object with various helpers for URL
 path operations. In this application a common pattern in paths is
 doctypes/<doctypeid>/fieldsets/<fiedsetid>/fields/<fieldid>. The path
 function below will take a source, which is a jQuery object, such as
 $('#some-id'), which has an attribute named 'data-group-id' having a
 value of the id of an element that stores data relevant to the current
 context as HTML data attributes, in particular the ids of doctypes,
 fieldsets and/or fields. The category is one of 'field', 'fieldset'
 or 'doctype'. The section argument is a section of the application,
 such as 'config' that will be prefixed to the path.
 
 Example:

 <pre> 
   <div
     id="someid"
     data-fieldset-fieldset="fsid"
     data-fieldset-doctype="did"></div>
     
   <div
    id="thisid"
    data-group-id="someid">
     
   mypath = path($('#thisid'), "fieldset");
   mypath.toString() == "doctypes/did/fieldsets/fsid";
   
   mypath = path($('#thisid'), "fieldset", "config");
   mypath.toString() == "config/doctypes/did/fieldsets/fsid";
   
   mypath = path($('#thisid'), "fieldset");
   mypath.fieldset = false; // unsets the fielset id
   mypath.toString() == "doctypes/did/fieldsets"; // all fieldsets
 </pre> 
   
 Note that the category matches the x of data-x in someid. Different
 values may be held for doctype or field in the same element. Sometimes
 this leads to repetition of information and a better scheme may be
 forthcoming. The positive side is that information about different
 paths may be held in the same location.

 h3. CouchDB Revision Numbers
 
 Above, a revision could have been added to someid as 'data-fieldset-rev'.
 
 h3. More Information
 
 For more information on how data attributes are used in this application,
 see getValue in the application.js file.
 
 h2. Manipulating the object
 
 Also note that setting certain path elements to false (or undefined)
 will exclude their ids from the end result. Setting the element to a
 different id would cause the path to be altered appropriately. This
 allows one to cleanly manipulate the paths without performing string
 manipulation.
 
 h2. PUT, POST and DELETE using the object
 
 There are also helpers for using the path the work with the resource it points to.
 
  Example:
 
 <pre> 
   mypath = path($('#thisid'), "fieldset");
   mypath.put(object, callback, context);
   mypath.post(object, callback, context);
   mypath.delete(callback, context);
 </pre>
   
 Object is an Javascript object that can be encoded as JSON, callback
 will be run on success and context provides information the environment
 from which the method was called, usually 'this' is supplied. (Context
 may no longer be an option in the future).
 
 The object will be sent to the path that would be returned by the
 toString method using the method implied by the above method's names.
 
 h3. Error handlers
 
 Within the context of this application it is assumed that fairly standard
 things will be done with error responces so they are left alone.
*/

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
      var value = getValue(prefix + item, path.origin);
      return value;
    })();
  });
  
  path.rev = getValue(prefix + 'rev', path.origin);
  
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
