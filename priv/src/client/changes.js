$(document).on("change", '#document-search-exclude', function (e) {
  shimi.searchui.toggleExclusion();
  return true;
});

$(document).on("change", '#document-search-invert', function (e) {
  shimi.searchui.toggleInversion();
  return true;
});