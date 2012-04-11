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

  lookup: function(item) {
    var stored = localStorage.getItem(item);
    if (stored === "") {
      return null;
    } else {
      return stored;
    }
  },

  excludedVal: function() {
    var exclude = $('#document-search-exclude').is(':checked');

    if (!exclude) {
      return null;
    } else {
      return exclude;
    }
  },

  toggleExclusion: function(e) {
    var exclude = searches.excludedVal();
    var excludeLabel = $('#search-exclude-label');

    if (exclude) {
      excludeLabel.show();
    } else {
      excludeLabel.hide();
    }

    localStorage.setItem("searchExclude", exclude);
  },

  clearSearchVals: function(initial) {
    $('#document-search-field').val('');
    $('#document-search-exclude:checked').click();
    $('.search-optional, #search-exclude-label').hide();
    $('.search-field-item').remove();
    if (!initial) {
      localStorage.setItem("searchLabels", null);
      localStorage.setItem("searchFields", null);
      localStorage.setItem("searchExclude", null);
    }
  },

  loadSearchVals: function() {
    var fieldids = searches.lookup("searchFields");

    if (fieldids) {
      $('#document-search-field').val(fieldids);
      $('#search-field-label').html(localStorage.getItem("searchLabels"));

      if (searches.lookup("searchExclude") != searches.excludedVal()) {
        $('#document-search-exclude').click();
      }

      $('.search-optional').show();
    }
  },

  updateSearchVals: function(fieldids, labels, exclude) {
    localStorage.setItem("searchLabels", labels);
    localStorage.setItem("searchFields", fieldids);
    localStorage.setItem("searchExclude", exclude);
  },

  removeSearchField: function(e) {
    var item = $(e.target);
    var value = item.attr('data-index');
    var searchField = $('#document-search-field');
    var currentVal = searchField.val();
    var valDecoded = JSON.parse(currentVal);
    var exclude = searches.excludedVal();
    var newVal = null;

    if (valDecoded.length === 1) {
      searches.clearSearchVals();
    } else {
      var index = valDecoded.indexOf(value);
      
      if (index >= 0) {
        valDecoded.splice(index, 1);
        newVal = JSON.stringify(valDecoded);
        searchField.val(JSON.stringify(valDecoded));
      }

      item.remove();
    }
    
    searches.updateSearchVals(newVal, $('#search-field-label').html(), exclude);
  },

  addSearchField: function(e) {
    var fieldid = $(e.target).closest('[data-field-field]')
      .attr('data-field-field');

    if (validID(fieldid)) {
      var fieldLabel = $(e.target).text();
      var searchField = $('#document-search-field');
      var currentVal = searchField.val();
      var searchLabel = $('#search-field-label');
      var exclude = searches.excludedVal();
      var newDecoded;
      var newVal = null;
      var newAnchor = '<a href="#" data-index="' + fieldid + 
        '" class="search-field-item" title="click to remove">' + 
        fieldLabel + '</a>';

      var setSearchVals = function(value) {
        if (searchLabel.html()) {
          searchLabel.children().last().after(newAnchor);
        } else {
          searchLabel.html(newAnchor);
        }
        
        newVal = JSON.stringify(value);
        searchField.val(newVal);
        searches.updateSearchVals(newVal, searchLabel.html(), exclude);
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
