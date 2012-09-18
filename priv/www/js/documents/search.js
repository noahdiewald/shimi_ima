var searches = {
  getSearch: function() {
    var query = $('#document-search-term').val();
    var url = "documents/search?q=" + encodeURIComponent(query);
    var field = $('#document-search-field').val();
    var exclude = $('#document-search-exclude').is(':checked');
    var index = $('#document-search-index').val();
    var lookup = searches.fieldLookup();

    if (index) {
      url = url + "&index=" + index; 
    } else {
      if (field) {
        url = url + "&field=" + field;
      }
      if (exclude) {
        url = url + "&exclude=true";
      }
    }

    $('#search-listing').hide();

    $.get(url, function(searchResults) {
            $('#search-listing').html(searchResults);
            $('.search-result-field-id')
              .each(function(index, item) {
                      var label = lookup[$(item).attr('data-field-field')];
                      var target = $(item).children('a').first();
                      target.html(label);
                      target.attr('data-search-label', label);
                    });
            $('.search-results th')
              .each(function(index, item) {
                      var itemText = $.trim($(item).children('a').html());
                      var re = new RegExp("(" + query + ")", "g");
                      var newText = 
                        itemText.replace(re, 
                                         "<span class='highlight'>$1</span>");
                      $(item).children('a').html(newText);
                    });
            $('#search-listing').show();
          });
  },

  fieldLookup: function() {
    var lookup = {};
    
    $('fieldset').each(
      function(index, fset) {
        var fsLabel = $(fset).attr('data-fieldset-label');
        $(fset).find('.field-container').each(
          function(index, item) {
            var id = $(item).attr('data-field-field');
            var label = $(item).find('.label-text').first().text();
            lookup[id] = fsLabel + ": " + label;
          });
      });
    return lookup;
  },

  lookup: function(item) {
    var stored = localStorage.getItem(item);
    if (stored === "" || stored === "null") {
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
    $('#document-search-field').val(null);
    $('#document-search-index').val(null);
    $('#document-search-exclude:checked').click();
    $('.search-optional, #search-exclude-label').hide();
    $('.search-field-item').remove();
    $('#search-index-label').empty();
    if (!initial) {
      localStorage.setItem("searchLabels", null);
      localStorage.setItem("searchFields", null);
      localStorage.setItem("searchExclude", null);
      localStorage.setItem("searchIndex", null);
      localStorage.setItem("searchIndexLabel", null);
    }
  },

  loadSearchVals: function() {
    var index = searches.lookup("searchIndex");
    var fieldids = searches.lookup("searchFields");

    if (index !== null) {
      $('#document-search-index').val(index);
      $('#search-index-label').html(localStorage.getItem("searchIndexLabel"));
      $('.search-optional').show();
      $('#document-search-exclude').parent('div').hide();
    } else if (fieldids !== null) {
      $('#document-search-field').val(fieldids);
      $('#search-field-label').html(localStorage.getItem("searchLabels"));

      if (searches.lookup("searchExclude") != searches.excludedVal()) {
        $('#document-search-exclude').click();
      }

      $('.search-optional').show();
    }
  },

  updateSearchVals: function(fieldids, labels, exclude, index) {
    if (index) {
      localStorage.setItem("searchIndex", index);
      localStorage.setItem("searchIndexLabel", labels);
      localStorage.setItem("searchLabels", null);
      localStorage.setItem("searchFields", null);
      localStorage.setItem("searchExclude", null);
    } else {
      localStorage.setItem("searchIndex", null);
      localStorage.setItem("searchIndexLabel", null);
      localStorage.setItem("searchLabels", labels);
      localStorage.setItem("searchFields", fieldids);
      localStorage.setItem("searchExclude", exclude);
    }
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

  addSearchIndex: function(e) {
    var indexVal = $('#index-index-input').val();
    var indexLabel = $('option[value=' + indexVal + ']').text();

    if (validID(indexVal)) {
      $('#search-all-fields-switch').show();
      $('#search-field-label').hide();
      $('#search-exclude-label').empty();
      $('#document-search-field').val(null);
      $('#document-search-exclude').parent('div').hide();
      $('#search-index-label').html(indexLabel).show();
      $('#document-search-index').val(indexVal);
      searches.updateSearchVals(null, indexLabel, null, indexVal);
    }
  },

  addSearchField: function(e) {
    var fieldid = $(e.target).closest('[data-field-field]')
      .attr('data-field-field');

    if (validID(fieldid)) {
      var fieldLabel = searches.fieldLookup()[fieldid];
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
