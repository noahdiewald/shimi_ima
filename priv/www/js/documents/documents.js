function loadHash(urlHash) {
  if (urlHash) {
    shimi.vui({id: urlHash}).get();
  }
  return false;
}

var searchAllFieldsSwitch = function() {
  $('#search-all-fields-switch a')
    .live("click", function() {
            shimi.sui.clearSearchVals();
          });
};

var searchFieldItems = function() {
  $('.search-field-item')
    .live("click", function(e) {
            shimi.sui.removeSearchField(e);
          });
};

var fieldViews = function() {
  $('.search-result-field-id a, .field-view b, .field-container label span')
    .live('dblclick', function(e) {
            shimi.sui.addSearchField(e);
          });
};

var searchIndex = function() {
  $('#index-index-input-label')
    .live('dblclick', function(e) {
            shimi.sui.addSearchIndex(e);
          });  
};

var excludeCheck = function() {
  $('#document-search-exclude')
    .live("change", function(e) {
            shimi.sui.toggleExclusion(e);
          });
};

var loadDocument = function(docid) {
  $("#document-view").html("<em>Loading...</em>");
  shimi.eui().clear();
  shimi.vui({id: docid}).get();
};

var jumpForm = function() {
  $('#view-jump-id')
    .live("keydown", 
          function(e) {
            if (e.which === 13) {
              var docid = $('#view-jump-id').val();
              loadDocument(docid);
            }
            return true;
          });  
};

var documentLinks = function() {
  // Allows the document for the listed item to be displayed
  // in the correct pane on click.
  $('.view-document-link')
    .live("click", 
          function () {
            loadDocument(this.hash.slice(1));
          });
};

var searchForm = function() {
  shimi.sui.clearSearchVals(true).loadSearchVals();
  searchAllFieldsSwitch();
  searchFieldItems();
  fieldViews();
  excludeCheck();
  searchIndex();
  $('#document-search-term')
    .live("keydown",
          function(e) {
            if (e.which === 13) {
              shimi.sui.getSearch();
              return false;
            }
            return true;
          });
};

$(
  function () {
    var getIndexTimer;
    
    documentLinks();
    shimi.iui().iOpts().get();
    jumpForm();
    searchForm();
    shimi.eui().init();

    $('#index-filter-form input').keyup(
      function() {
        clearTimeout(getIndexTimer);
        getIndexTimer = setTimeout(function () {shimi.iui().get();}, 500);
      });
  
    $('#index-filter-form select').change(
      function() {
        shimi.iui().get();
      });
  
    loadHash($(location)[0].hash.split("#")[1]);
  });
