shimi.loadHash = function(urlHash) {
  if (urlHash) {
    shimi.vui({id: urlHash}).get();
  }
  return false;
};

shimi.searchAllFieldsSwitch = function() {
  $('#search-all-fields-switch a')
    .live("click", function() {
            shimi.sui.clearSearchVals();
          });
};

shimi.searchFieldItems = function() {
  $('.search-field-item')
    .live("click", function(e) {
            shimi.sui.removeSearchField(e);
          });
};

shimi.fieldViews = function() {
  $('.search-result-field-id a, .field-view b, .field-container label span')
    .live('dblclick', function(e) {
            shimi.sui.addSearchField(e);
          });
};

shimi.searchIndex = function() {
  $('#index-index-input-label')
    .live('dblclick', function(e) {
            shimi.sui.addSearchIndex(e);
          });  
};

shimi.excludeCheck = function() {
  $('#document-search-exclude')
    .live("change", function(e) {
            shimi.sui.toggleExclusion(e);
          });
};

shimi.loadDocument = function(docid) {
  $("#document-view").html("<em>Loading...</em>");
  shimi.eui().clear();
  shimi.vui({id: docid}).get();
};

shimi.jumpForm = function() {
  $('#view-jump-id')
    .live("keydown", 
          function(e) {
            if (e.which === 13) {
              var docid = $('#view-jump-id').val();
              shimi.loadDocument(docid);
            }
            return true;
          });  
};

shimi.documentLinks = function() {
  // Allows the document for the listed item to be displayed
  // in the correct pane on click.
  $('.view-document-link')
    .live("click", 
          function () {
            shimi.loadDocument(this.hash.slice(1));
          });
};

shimi.searchForm = function() {
  shimi.sui().clearSearchVals(true).loadSearchVals();
  shimi.searchAllFieldsSwitch();
  shimi.searchFieldItems();
  shimi.fieldViews();
  shimi.excludeCheck();
  shimi.searchIndex();
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
