// Helper for building the url to access a fieldset for a document
function buildUrl(project, doctype, fieldset) {
  return "/projects/" + project +
         "/doctypes/" + doctype +
         "/fieldsets/" + fieldset;
}

function dpath(source, category) {
  var url = path(source, category);
  url.doctype = false;
  return url;
}

function fsInfo(key, elem) {
  return getData("fieldset-" + key, elem);
}

function fInfo(key, elem) {
  return getData("field-" + key, elem);
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
});
