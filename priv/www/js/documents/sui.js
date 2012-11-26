shimi.sui = function() {
  var mod = {};
  var utils = shimi.utils();
  var localStorage = window.localStorage;
  var dSearchIndex = $('#document-search-index');
  var dSearchTerm = $('#document-search-term');
  var dSearchField = $('#document-search-field');
  var dSearchExclude = $('#document-search-exclude');
  var searchListing = $('#search-listing');

  var fieldLookup = function() {
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
  };

  var lookup = function(item) {
    var stored = localStorage.getItem(item);
    if (stored === "" || stored === "null") {
      return null;
    } else {
      return stored;
    }
  };

  var excludedVal = function() {
    var exclude = dSearchExclude.is(':checked');

    if (!exclude) {
      return null;
    } else {
      return exclude;
    }
  };

  var updateSearchVals = function(fieldids, labels, exclude, index) {
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
    
    return true;
  };
  
  mod.getSearch = function() {
    var query = dSearchTerm.val();
    var url = "documents/search?q=" + window.encodeURIComponent(query);
    var field = dSearchField.val();
    var exclude = dSearchExclude.is(':checked');
    var index = dSearchIndex.val();
    var lookup = fieldLookup();

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

    searchListing.hide();

    $.get(url, function(searchResults) {
            searchListing.html(searchResults);
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
            searchListing.show();
          });
          
    return mod;
  };

  mod.toggleExclusion = function(e) {
    var exclude = mod.excludedVal();
    var excludeLabel = $('#search-exclude-label');

    if (exclude) {
      excludeLabel.show();
    } else {
      excludeLabel.hide();
    }

    localStorage.setItem("searchExclude", exclude);
    
    return mod;
  };

  mod.clearSearchVals = function(initial) {
    dSearchField.val(null);
    dSearchIndex.val(null);
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
    
    return mod;
  };

  mod.loadSearchVals = function() {
    var index = lookup("searchIndex");
    var fieldids = lookup("searchFields");

    if (index !== null) {
      dSearchIndex.val(index);
      $('#search-index-label').html(localStorage.getItem("searchIndexLabel"));
      $('.search-optional').show();
      dSearchExclude.parent('div').hide();
    } else if (fieldids !== null) {
      dSearchField.val(fieldids);
      $('#search-field-label').html(localStorage.getItem("searchLabels"));

      if (lookup("searchExclude") !== mod.excludedVal()) {
        dSearchExclude.click();
      }

      $('.search-optional').show();
    }
    
    return mod;
  };

  mod.removeSearchField =  function(e) {
    var item = $(e.target);
    var value = item.attr('data-index');
    var searchField = dSearchField;
    var currentVal = searchField.val();
    var valDecoded = JSON.parse(currentVal);
    var exclude = mod.excludedVal();
    var newVal = null;

    if (valDecoded.length === 1) {
      mod.clearSearchVals();
    } else {
      var index = valDecoded.indexOf(value);
      
      if (index >= 0) {
        valDecoded.splice(index, 1);
        newVal = JSON.stringify(valDecoded);
        searchField.val(JSON.stringify(valDecoded));
      }

      item.remove();
    }
    
    updateSearchVals(newVal, $('#search-field-label').html(), exclude);
    
    return mod;
  };

  mod.addSearchIndex = function(e) {
    var indexVal = $('#index-index-input').val();
    var indexLabel = $('option[value=' + indexVal + ']').text();

    if (utils.validID(indexVal)) {
      $('#search-all-fields-switch').show();
      $('#search-field-label').hide();
      $('#search-exclude-label').empty();
      dSearchField.val(null);
      dSearchExclude.parent('div').hide();
      $('#search-index-label').html(indexLabel).show();
      dSearchIndex.val(indexVal);
      updateSearchVals(null, indexLabel, null, indexVal);
    }
    
    return mod;
  };

  mod.addSearchField = function(e) {
    var fieldid = $(e.target).closest('[data-field-field]')
      .attr('data-field-field');

    if (utils.validID(fieldid)) {
      var fieldLabel = fieldLookup()[fieldid];
      var searchField = dSearchField;
      var currentVal = searchField.val();
      var searchLabel = $('#search-field-label');
      var exclude = mod.excludedVal();
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
        updateSearchVals(newVal, searchLabel.html(), exclude);
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
    
    return mod;
  };
  
  return mod;
};
