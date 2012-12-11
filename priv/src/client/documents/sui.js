shimi.sui = (function () {
  var mod = {};
  var utils = shimi.utils();
  var localStorage = window.localStorage;
  var dSearchIndex = function () {
    return $('#document-search-index');
  };
  var dSearchTerm = function () {
    return $('#document-search-term');
  };
  var dSearchField = function () {
    return $('#document-search-field');
  };
  var dSearchExclude = function () {
    return $('#document-search-exclude');
  };
  var dSearchInvert = function () {
    return $('#document-search-invert');
  };
  var searchListing = function () {
    return $('#search-listing');
  };

  var fieldLookup = function () {
    var lookup = {};

    $('fieldset').each(

    function (index, fset) {
      var fsLabel = $(fset).attr('data-fieldset-label');
      $(fset).find('.field-container').each(

      function (index, item) {
        var id = $(item).attr('data-field-field');
        var label = $(item).find('.label-text').first().text();
        lookup[id] = fsLabel + ": " + label;
      });
    });
    return lookup;
  };

  var lookup = function (item) {
    var stored = localStorage.getItem(item);
    if (stored === "" || stored === "null") {
      return null;
    } else {
      return stored;
    }
  };

  var excludedVal = function () {
    var exclude = dSearchExclude().is(':checked');

    if (!exclude) {
      return null;
    } else {
      return exclude;
    }
  };

  var invertedVal = function () {
    var invert = dSearchInvert().is(':checked');

    if (!invert) {
      return null;
    } else {
      return invert;
    }
  };

  var updateSearchVals = function (fieldids, labels, exclude, index, invert) {
    if (index) {
      localStorage.setItem("searchIndex", index);
      localStorage.setItem("searchIndexLabel", labels);
      localStorage.setItem("searchLabels", null);
      localStorage.setItem("searchFields", null);
      localStorage.setItem("searchExclude", null);
      localStorage.setItem("searchInvert", invert);
    } else {
      localStorage.setItem("searchIndex", null);
      localStorage.setItem("searchIndexLabel", null);
      localStorage.setItem("searchLabels", labels);
      localStorage.setItem("searchFields", fieldids);
      localStorage.setItem("searchExclude", exclude);
      localStorage.setItem("searchInvert", invert);
    }

    return true;
  };

  mod.toggleInversion = function () {
    localStorage.setItem("searchInvert", invertedVal());
    return mod;
  };

  mod.toggleExclusion = function () {
    localStorage.setItem("searchExclude", excludedVal());
    return mod;
  };

  mod.getSearch = function () {
    var query = dSearchTerm().val();
    var url = "documents/search?q=" + window.encodeURIComponent(query);
    var field = dSearchField().val();
    var exclude = dSearchExclude().is(':checked');
    var invert = dSearchInvert().is(':checked');
    var index = dSearchIndex().val();
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
    if (invert) {
      url = url + "&invert=true";
    }

    searchListing().hide();

    $.get(url, function (searchResults) {
      searchListing().html(searchResults);
      $('.search-result-field-id').each(function (index, item) {
        var label = lookup[$(item).attr('data-field-field')];
        var target = $(item).children('a').first();
        target.html(label);
        target.attr('data-search-label', label);
      });
      if (!invert) {
        $('.search-results th').each(function (index, item) {
          var itemText = $.trim($(item).children('a').html());
          var re = new RegExp("(" + query + ")", "g");
          var newText =
          itemText.replace(re, "<span class='highlight'>$1</span>");
          $(item).children('a').html(newText);
        });
      }
      searchListing().show();
    });

    return mod;
  };

  mod.clearSearchVals = function (initial) {
    dSearchField().val(null);
    dSearchIndex().val(null);
    $('#document-search-exclude:checked').click();
    $('#document-search-invert:checked').click();
    $('.search-field-item').remove();
    $('#search-index-label').empty();
    if (!initial) {
      localStorage.setItem("searchLabels", null);
      localStorage.setItem("searchFields", null);
      localStorage.setItem("searchExclude", null);
      localStorage.setItem("searchInvert", null);
      localStorage.setItem("searchIndex", null);
      localStorage.setItem("searchIndexLabel", null);
    }

    return mod;
  };

  mod.loadSearchVals = function () {
    var index = lookup("searchIndex");
    var fieldids = lookup("searchFields");

    if (index !== null) {
      dSearchIndex().val(index);
      $('#search-index-label').html(localStorage.getItem("searchIndexLabel"));

      $('.search-optional').show();
      dSearchExclude().parent('div').hide();
    } else if (fieldids !== null) {
      dSearchField().val(fieldids);
      $('#search-field-label').html(localStorage.getItem("searchLabels"));

      if (lookup("searchExclude") !== excludedVal()) {
        dSearchExclude().click();
      }

      $('.search-optional').show();

      if (fieldids.length === 36) {
        if (lookup("searchInvert") !== invertedVal()) {
          dSearchInvert().click();
        }
      } else {
        dSearchInvert().parent('div').hide();
      }
    }

    return mod;
  };

  mod.removeSearchField = function (target) {
    var value = target.attr('data-index');
    var searchField = dSearchField();
    var currentVal = searchField.val();
    var valDecoded = JSON.parse(currentVal);
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

      target.remove();
    }

    if (valDecoded.length === 1 && newVal !== null) {
      $('.search-only-one').show();
    } else {
      if (invertedVal()) {
        dSearchInvert().click();
      }
      $('.search-only-one').hide();
    }

    updateSearchVals(newVal, $('#search-field-label').html(), excludedVal(), null, invertedVal());

    return mod;
  };

  mod.addSearchIndex = function () {
    var indexVal = $('#index-index-input').val();
    var indexLabel = $('option[value=' + indexVal + ']').text();

    if (utils.validID(indexVal)) {
      mod.clearSearchVals();
      $('#search-index-label').html(indexLabel).show();
      dSearchIndex().val(indexVal);
      $('.search-only-one').show();
      updateSearchVals(null, indexLabel, null, indexVal, invertedVal());
    }

    return mod;
  };

  mod.addSearchField = function (target) {
    var fieldid = $(target).closest('[data-field-field]').attr('data-field-field');

    if (dSearchIndex().val()) {
      mod.clearSearchVals();
    }

    if (utils.validID(fieldid)) {
      var fieldLabel = fieldLookup()[fieldid];
      var searchField = dSearchField();
      var currentVal = searchField.val();
      var searchLabel = $('#search-field-label');
      var newDecoded;
      var newVal = null;
      var newAnchor = '<a href="#" data-index="' + fieldid + '" class="search-field-item" title="click to remove">' + fieldLabel + '</a>';

      var setSearchVals = function (value) {
        if (searchLabel.html()) {
          searchLabel.children().last().after(newAnchor);
        } else {
          searchLabel.html(newAnchor);
        }

        newVal = JSON.stringify(value);
        searchField.val(newVal);

        if (value.length === 1) {
          $('.search-only-one').show();
        } else {
          $('.search-only-one').hide();
        }

        updateSearchVals(newVal, searchLabel.html(), excludedVal(), null, invertedVal());
      };

      if (currentVal !== '') {
        var valDecoded = JSON.parse(currentVal);
        if (valDecoded.indexOf(fieldid) < 0) {
          newDecoded = valDecoded.concat(fieldid);
          $('.search-optional').show();
          setSearchVals(newDecoded);
        }
      } else {
        newDecoded = [fieldid];
        $('.search-optional').show();
        setSearchVals(newDecoded);
      }

    }

    return mod;
  };

  return mod;
})();