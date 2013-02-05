shimi.loadHash = function (urlHash) {
  if (urlHash) {
    shimi.viewui.get(urlHash);
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
      shimi.viewui.get(docid);
    }
    return true;
  });
};

shimi.setForm = function () {
  $('#new-set-form').on("submit", function () {
    return false;
  });
  $('#document-sets-form').on("keydown", function (e) {
    if (e.which === 13) {
      shimi.setsui.performOp();
      return false;
    }
    return true;
  });
  shimi.setsui.updateSelection();
};

shimi.searchForm = function () {
  shimi.searchui.loadSearchVals();
  $('#document-search-form').on("submit", function () {
    return false;
  });
  $('#document-search-term').on("keydown", function (e) {
    if (e.which === 13) {
      shimi.searchui.getSearch();
      return false;
    }
    return true;
  });
};