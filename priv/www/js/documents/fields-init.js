function initFieldsets() {
  $('fieldset').each(function(i, fieldset) {
    initFieldset(fieldset);
  });
  
  return true;
}

function initFieldset(fieldset, callback) {
  var url = dpath($(fieldset), "fieldset");
  var container = $('#container-' + fieldset.id);
  
  $.get(url.toString(), function(data) {
    container.append(data);
    initFields(container, callback);
  });
  
  return false;
}

function initFields(container, callback) {
  var url = dpath(container, "field");
  var section = container.children('.fields').last();
  
  $.get(url.toString(), function(data) {
    section.prepend(data);
    if (callback) {
      callback(section);
    }

    afterFreshRefresh();
  });
  
  return true;
}

