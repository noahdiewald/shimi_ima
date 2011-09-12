// Helper for building the url to access a fieldset for a document
function buildUrl(project, doctype, fieldset) {
  return "/projects/" + project +
         "/doctypes/" + doctype +
         "/fieldsets/" + fieldset;
}

function fsContainer(id) {
  return $("#container-" + id);
}

function dpath(source, category) {
  var url = path(source, category);
  url.doctype = false;
  return url;
}

function fsInfo(key, elem) {
  return getValue("fieldset-" + key, elem);
}

function fInfo(key, elem) {
  return getValue("field-" + key, elem);
}

function dInfo(key, elem) {
  return getValue("document-" + key, elem);
}

function loadHash(urlHash) {
  if (urlHash) getDocument(urlHash);
}

$(function () {
  $('body').click(function(e) {clickDispatch(e)});
  
  fillQueryOptions();
  getIndex();
  initEdit();
  
  $('#index-filter-form input').keyup(function() {
    getIndex();
  });
  
  $('#index-filter-form select').change(function() {
    getIndex();
  });
  
  loadHash($(location)[0].hash.split("#")[1]);
});
