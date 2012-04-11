var searches = {
  getSearch: function() {
    var query = $('#document-search-term').val();
    var url = "documents/search?q=" + encodeURIComponent(query);
    var field = $('#document-search-field').val();
    var exclude = $('#document-search-exclude').is(':checked');
    var lookup = searches.fieldLookup();

    if (field) {
      url = url + "&field=" + field;
    }

    if (exclude) {
      url = url + "&exclude=true";
    }

    $('#search-listing').hide();

    $.get(url, function(searchResults) {
            $('#search-listing').html(searchResults);
            $('.search-result-field-id')
              .each(function(index, item) {
                      var label = lookup[$(item).attr('data-field-field')];
                      $(item).children('a').html(label);
                    });
            $('#search-listing').show();
          });
  },

  fieldLookup: function() {
    var lookup = {};
    
    $('.field-container')
      .each(function(index, item) {
              var id = $(item).attr('data-field-field');
              var label = $(item).find('.label-text')
                .first().text();
              lookup[id] = label;
            });
    return lookup;
  },

  toggleExclusion: function(e) {
    var exclude = $('#document-search-exclude').is(':checked');
    var excludeLabel = $('#search-exclude-label');

    if (exclude) {
      excludeLabel.show();
    } else {
      excludeLabel.hide();
    }
  },

  clearSearchVals: function(e) {
    $('#document-search-field').val('');
    $('#document-search-exclude:checked').click();
    $('.search-optional, #search-exclude-label').hide();
    $('.search-field-item').remove();
  },

  removeSearchField: function(e) {
    var item = $(e.target);
    var value = item.attr('data-index');
    var searchField = $('#document-search-field');
    var currentVal = searchField.val();
    var valDecoded = JSON.parse(currentVal);
    
    if (valDecoded.length === 1) {
      searches.clearSearchVals();
    } else {
      var index = valDecoded.indexOf(value);
      
      if (index >= 0) {
        valDecoded.splice(index, 1);
        searchField.val(JSON.stringify(valDecoded));
      }

      item.remove();
    }
  },

  addSearchField: function(e) {
    var fieldid = $(e.target).closest('[data-field-field]')
      .attr('data-field-field');

    if (validID(fieldid)) {
      var fieldLabel = $(e.target).text();
      var searchField = $('#document-search-field');
      var currentVal = searchField.val();
      var searchLabel = $('#search-field-label');
      var newIndex;
      var newDecoded;
      var newAnchor = '<a href="#" data-index="' + fieldid + 
        '" class="search-field-item" title="click to remove">' + 
        fieldLabel + '</a>';
      var setSearchVals = function(value) {
        if (searchLabel.html()) {
          searchLabel.children().last().after(newAnchor);
        } else {
          searchLabel.html(newAnchor);
        }

        searchField.val(JSON.stringify(value));
      };

      if (currentVal !== '') {
        var valDecoded = JSON.parse(currentVal);
        if (valDecoded.indexOf(fieldid) < 0) {
          newDecoded = valDecoded.concat(fieldid);
          setSearchVals(newDecoded);
        }
      } else {
        newDecoded = [fieldid];      
        setSearchVals(newDecoded);
      }

      $('.search-optional').show();
    }
  }
};
