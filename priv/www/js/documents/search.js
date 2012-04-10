var getSearch = function(query) {
  var url = "documents/search?q=" + encodeURIComponent(query);

  $.get(url, function(searchResults) {
          $('#search-listing').html(searchResults);
        });
};