shimi.loadHash = function (urlHash) {
  if (urlHash) {
    shimi.vui.get(urlHash);
  }
  return false;
};

shimi.jumpForm = function () {
  $('#view-jump-id').live("keydown", function (e) {
    if (e.which === 13) {
      var docid = $('#view-jump-id').val();
      shimi.vui.get(docid);
    }
    return true;
  });
};

shimi.searchForm = function () {
  shimi.sui.clearSearchVals(true).loadSearchVals();
  $('#document-search-term').live("keydown", function (e) {
    if (e.which === 13) {
      shimi.sui.getSearch();
      return false;
    }
    return true;
  });
};