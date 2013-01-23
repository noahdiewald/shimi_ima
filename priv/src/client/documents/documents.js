shimi.loadHash = function (urlHash) {
  if (urlHash) {
    shimi.vui.get(urlHash);
  }
  return false;
};

shimi.jumpForm = function () {
  $('#view-jump').live("submit", function () {
    return false;
  });
  $('#view-jump-id').live("keypress", function (e) {
    if (e.which === 13) {
      var docid = $('#view-jump-id').val();
      shimi.vui.get(docid);
    }
    return true;
  });
};

shimi.searchForm = function () {
  shimi.sui.loadSearchVals();
  $('#document-search-form').on("submit", function () {
    return false;
  });
  $('#document-search-term').on("keydown", function (e) {
    if (e.which === 13) {
      shimi.sui.getSearch();
      return false;
    }
    return true;
  });
};