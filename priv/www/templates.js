this["JST"] = this["JST"] || {};

this["JST"]["priv/templates/set-listing"] = function(obj){
var __p='';var print=function(){__p+=Array.prototype.join.call(arguments, '')};
with(obj||{}){
__p+='<div class="total-rows-info">\n  <b>Total</b>: <span id="total-set-rows">'+
_.escape( total )+
'</span>\n</div>\n<div id="save-set-results">\n  <a href="#">(Save Selected)</a>\n</div>\n<table id="set-elements">\n  <thead>\n    <tr>\n      <td>\n        <input type="checkbox" id="select-all-set-elements" title="Click to select or deselect all elements" />\n      </td>\n      <th>\n        Elements\n      </th>\n    </tr>\n  </thead>\n  <tbody>\n    ';
 _.each(elements, function(elem) { 
;__p+='\n    <tr>\n      <td>\n        <input type="checkbox" class="set-element-selection" title="Click to select element" />\n      </td>\n      <td>\n        <a class="view-document-link" href="#'+
_.escape( elem.id )+
'">'+
_.escape( elem.context )+
'</a>\n      </td>\n    </tr>\n    ';
 }); 
;__p+='\n  </tbody>\n</table>\n';
}
return __p;
};

this["JST"]["priv/templates/set-options"] = function(obj){
var __p='';var print=function(){__p+=Array.prototype.join.call(arguments, '')};
with(obj||{}){
__p+='<option></option>\n';
 names.forEach(function(name) { 
;__p+='\n<option value="'+
_.escape( name )+
'">'+
_.escape( name )+
'</option>\n';
 }); 
;__p+='\n';
}
return __p;
};

this["JST"]["priv/templates/search-field-item"] = function(obj){
var __p='';var print=function(){__p+=Array.prototype.join.call(arguments, '')};
with(obj||{}){
__p+='<a class=\'search-field-item\' \n  title=\'click to remove\' \n  data-field-field=\''+
_.escape( field )+
'\' \n  href=\'#\'>'+
_.escape( fieldLabel )+
'</a>\n';
}
return __p;
};

this["JST"]["priv/templates/index-listing"] = function(obj){
var __p='';var print=function(){__p+=Array.prototype.join.call(arguments, '')};
with(obj||{}){
__p+='<ul>\n  ';
 _.each(rows, function(row) { 
;__p+='\n  <li><a href="#" data-index-id="'+
_.escape( row.id )+
'">'+
_.escape( row.key[1] )+
'<span class="quiet">('+
_.escape( row.key[0] )+
')</span></a></li>\n  ';
 }); 
;__p+='\n</ul>\n';
}
return __p;
};

this["JST"]["priv/templates/index-options"] = function(obj){
var __p='';var print=function(){__p+=Array.prototype.join.call(arguments, '')};
with(obj||{}){
__p+='<option></option>\n';
 _.each(rows, function(row) { 
;__p+='\n<option value="'+
_.escape( row.id )+
'">'+
_.escape( row.key[1] )+
'</option>\n';
 }); 
;__p+='\n';
}
return __p;
};