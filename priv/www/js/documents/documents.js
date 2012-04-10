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

var jumpForm = function() {
  $('#view-jump-id')
    .on("keydown", 
          function(e) {
            if (e.which === 13) {
              var docid = $('#view-jump-id').val();
              loadDocument(docid);
            }
            return true;
          });  
};

var searchForm = function() {
  $('#document-search-term')
    .on("keydown",
          function(e) {
            if (e.which === 13) {
              var query = $('#document-search-term').val();
              getSearch(query);
              return false;
            }
            return true;
          });
};

var loadSearchResults = function(query) {
  getSearch(query);
};

var loadDocument = function(docid) {
  $("#document-view").html("<em>Loading...</em>");
  clearDoc();
  getDocument(docid);
};

var documentLinks = function() {
  // Allows the document for the listed item to be displayed
  // in the correct pane on click.
  $('.view-document-link')
    .on("click", 
          function () {loadDocument(this.hash.slice(1));});
};

$(
  function () {
    var getIndexTimer;

    $('body').click(function(e) {clickDispatch(e);});
    
    documentLinks();
    fillQueryOptions();
    getIndex();
    jumpForm();
    searchForm();
    initEdit();

    $('#index-filter-form input').keyup(
      function() {
        clearTimeout(getIndexTimer);
        getIndexTimer = setTimeout(function () {getIndex();}, 500);
      });
  
    $('#index-filter-form select').change(
      function() {
        getIndex();
      });
  
    loadHash($(location)[0].hash.split("#")[1]);
  });
