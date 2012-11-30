shimi.loadHash = function(urlHash) {
  if (urlHash) {
    shimi.vui({id: urlHash}).get();
  }
  return false;
};

shimi.excludeCheck = function() {
  $('#document-search-exclude')
    .live("change", function() {
            shimi.sui.toggleExclusion();
          });
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

shimi.searchForm = function() {
  shimi.sui.clearSearchVals(true).loadSearchVals();
  shimi.excludeCheck();
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
