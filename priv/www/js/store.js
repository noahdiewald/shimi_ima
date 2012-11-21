/*
 WARNING: OUT OF DATE
 
 h1. Data Attribute Key Value Stores
 
 There are two functions provided for getting values from keys
 embedded in HTML elements.
  
  @getValue(key, elem)@
  
  This funtion takes a key that corresponds to the name of the data
  attribute without the data- prefix. It also takes a jQuery
  object. The assumption is that the jQuery object will hold only one
  element but it may work either way. The element is expected to have
  an attribute data-group-id with a value that is the id of the
  element actually holding the data.
 
 Example:

 <pre> 
   <div
     id="someid"
     data-fieldset-fieldset="fsid"
     data-fieldset-doctype="did"></div>
     
   <div
    id="thisid"
    data-group-id="someid">
    
   getValue("fieldset-doctype", $(thisid)) == "did";
 </pre> 
   
 The following also works:

 <pre> 
   <div
     id="someid2"
     data-fieldset-fieldset="fsid"
     data-fieldset-doctype="did"></div>
   
   <div
     id="someid"
     data-group-id="someid2"
     data-fieldset-fieldset="fsid"></div>
     
   <div
    id="thisid"
    data-group-id="someid"></div>
    
   getValue("fieldset-doctype", $(thisid)) == "did";
 </pre> 
   
  @putValue(key, value, elem)@
  
  This function will set an attribute at the target with a name
  corresponding to key and a value of value.
*/

/*
 Tail call optimization taken from Spencer Tipping's Javascript in Ten
 Minutes.

 For more information see:
 https://github.com/spencertipping/js-in-ten-minutes
*/

var identity = function(x) {return x;};

Function.prototype.r = function() {return [this, arguments];};

Function.prototype.t = function() {
  var c = [this, arguments];
  var escape = arguments[arguments.length - 1];
  while (c[0] !== escape) {
    c = c[0].apply(this, c[1]);
  }
  return escape.apply(this, c[1]);
};

var store = function(elem) {
  var mod = {};
  
  mod.get = function(key) {
    var getValue1 = function(key, elem, id) {
      var gid = elem.attr('data-group-id');
      var store = $('#' + gid);
      var val = store.attr('data-' + key);
      var next = store.attr('data-group-id');
      
      if (val === undefined &&  next !== undefined &&  gid !== next) {
        return getValue1.r(key, store, id);
      }
      
      return id.r(val);
    };
    
    return getValue1.t(key, elem, identity);
  };
  
  mod.put = function(key, value) {
    var dataElem = elem.attr('data-group-id');
    $('#' + dataElem).attr('data-' + key, value);
  };
  
  mod.fs = function(key) {
    return mod.get("fieldset-" + key);
  };
  
  mod.f = function(key) {
    return mod.get("field-" + key);
  };
  
  mod.d = function(key) {
    return mod.get("document-" + key);
  };
  
  return mod;
};