var getSearch = function(query) {
  var url = "documents/search?q=" + encodeURIComponent(query);
  var field = $('#document-search-field').val();

  if (field) {
    url = url + "&field=" + field;
  }

  $.get(url, function(searchResults) {
          $('#search-listing').html(searchResults);
        });
};