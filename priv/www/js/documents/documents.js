// Helper for building the url to access a fieldset for a document
function buildUrl(project, doctype, fieldset) {
  return "/projects/" + project +
         "/doctypes/" + doctype +
         "/fieldsets/" + fieldset;
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
