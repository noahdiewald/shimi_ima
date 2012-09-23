
var ifStoredElse = function(key, success, otherwise) {
  var item = null;

  item = localStorage.getItem(key);

  if (item) {
    success(item);
  } else {
    $.get(key, otherwise);
  }
};

var initFieldsets = function() {
  var reload;
  var container = $("#create-document-button");
  var doctype = dInfo("doctype", container);
  var versionKey = doctype + "_version";
  var oldVersion = localStorage.getItem(versionKey);
  var curVersion = dInfo("version", container);

  reload = oldVersion != curVersion;
  localStorage.setItem(versionKey, curVersion);

  $('fieldset').each(function(i, fieldset) {
    initFieldset(fieldset, false, reload);
  });
  
  return true;
}

var initFieldset = function(fieldset, callback, reload) {
  var url = dpath($(fieldset), "fieldset").toString();
  var id = fsInfo("fieldset", $(fieldset));
  var container = $('#container-' + id);
  var appendIt = function(data) {
    container.append(data);
    initFields(container, callback, reload);
  };
  var storeIt = function(data) {
    localStorage.setItem(url, data);
    appendIt(data);
  };

  if (reload) {
    localStorage.removeItem(url);
  }

  ifStoredElse(url.toString(), appendIt, storeIt);

  return false;
}

var initFields = function(container, callback, reload) {
  var url = dpath(container, "field");
  var section = container.children('.fields').last();
  var prependIt = function(data) {
    section.prepend(data);
    if (callback) {
      callback(section);
    }
    
    afterFreshRefresh();
  };
  var storeIt = function(data) {
    localStorage.setItem(url, data);
    prependIt(data);
  };

  if (reload) {
    localStorage.removeItem(url);
  }

  ifStoredElse(url.toString(), prependIt, storeIt);
    
  return true;
}
