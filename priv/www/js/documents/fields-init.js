function initFieldsets() {
  $('fieldset').each(function(i, fieldset) {
    initFieldset(fieldset);
  });
  
  return true;
}

function initFieldset(fieldset, callback) {
  var url = dpath($(fieldset), "fieldset");
  var id = fsInfo("fieldset", $(fieldset));
  var container = $('#container-' + id);
  var appendIt = function(data) {
    container.append(data);
    initFields(container, callback);
  };
  var storeIt = function(data) {
    localStorage.setItem(url, data);
    appendIt(data);
  };

  ifStoredElse(url.toString(), appendIt, storeIt);

  return false;
}

var ifStoredElse = function(key, success, otherwise) {
  var item = null;

  item = localStorage.getItem(key);

  if (item) {
    success(item);
  } else {
    $.get(key, otherwise);
  }
};

function initFields(container, callback) {
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

  ifStoredElse(url.toString(), prependIt, storeIt);
    
  return true;
}

