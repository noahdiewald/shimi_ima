shimi.searchui = (function () {
  var mod = {};
  var utils = shimi.utils();
  var sets = shimi.sets;
  var setsui = shimi.setsui;
  var localStorage = window.localStorage;
  var searchIndex = function () {
    return $('#document-search-index');
  };
  var searchIndexLabel = function () {
    return $('#search-index-label');
  };
  var searchTerm = function () {
    return $('#document-search-term');
  };
  var searchFields = function () {
    return $('#document-search-field');
  };
  var searchFieldsLabel = function () {
    return $('#search-field-label');
  };
  var searchExclude = function () {
    return $('#document-search-exclude');
  };
  var searchInvert = function () {
    return $('#document-search-invert');
  };
  var searchAll = function () {
    return $('#search-all-fields-switch');
  };
  var searchListing = function () {
    return $('#search-listing');
  };
  var getIdentifier = function () {
    return shimi.documents.identifier();
  };
  var formElems = [searchIndex, searchIndexLabel, searchFields, searchFieldsLabel, searchExclude, searchInvert, searchAll];

  var indexVal = function () {
    var val = $("#index-index-input").val();
    if (val.length === 0) {
      return null;
    } else {
      return val;
    }
  };

  var maybeTrue = function (bool) {
    if (bool) {
      return true;
    } else {
      return null;
    }
  };

  var clearStore = function () {
    var ident = getIdentifier();
    localStorage.setItem(ident + "_searchIndex", null);
    localStorage.setItem(ident + "_searchIndexLabel", null);
    localStorage.setItem(ident + "_searchFields", null);
    localStorage.setItem(ident + "_searchExclude", null);
    localStorage.setItem(ident + "_searchInvert", null);
  };

  var clearVals = function () {
    formElems.forEach(function (x) {
      var elem = x();
      switch (elem.attr('type')) {
      case "hidden":
        elem.val('');
        break;
      case "checkbox":
        elem.attr("checked", false);
        break;
      }
    });
  };

  var hideElems = function () {
    formElems.forEach(function (x) {
      var elem = x();
      switch (elem.attr('type')) {
      case "hidden":
        break;
      case "checkbox":
        elem.parent("div").hide();
        break;
      default:
        elem.hide();
      }
    });
  };

  var fieldLabels = function () {
    var ident = getIdentifier();
    var fieldlabels = JSON.parse(sessionStorage.getItem(ident + "_labels"));
    return fieldlabels;
  };

  var searchFieldItem = function (field, fieldLabel) {
    return templates['search-field-item'].render({
      fieldLabel: fieldLabel,
      field: field
    });
  };

  var setFields = function (fields) {
    var fLabels = fieldLabels();
    var jFields = JSON.stringify(fields);
    var sfls = searchFieldsLabel();
    var ident = getIdentifier();

    searchFields().val(jFields);
    localStorage.setItem(ident + "_searchFields", jFields);

    var linkLabels = fields.map(function (x) {
      return searchFieldItem(x, fLabels[x].join(": "));
    });

    sfls.html(linkLabels.join(" "));

    return true;
  };

  mod.allFields = function () {
    clearStore();
    hideElems();
    clearVals();
    return mod;
  };

  mod.singleField = function (fields) {
    mod.multipleFields(fields);
    searchInvert().parent().show();
    return mod;
  };

  mod.singleFieldInverse = function (fields) {
    var ident = getIdentifier();
    mod.singleField(fields);
    searchInvert().attr('checked', true);
    localStorage.setItem(ident + "_searchInvert", true);
    return mod;
  };

  mod.multipleFields = function (fields) {
    mod.allFields();
    setFields(fields);
    [searchAll(), searchFieldsLabel(), searchExclude().parent()].forEach(function (x) {
      x.show();
    });
    return mod;
  };

  mod.excludedFields = function (fields) {
    var ident = getIdentifier();
    if (fields.length > 1) {
      mod.multipleFields(fields);
    } else {
      mod.singleField(fields);
    }
    searchExclude().attr('checked', true);
    localStorage.setItem(ident + "_searchExclude", true);
    return mod;
  };

  mod.indexOnly = function (index, indexLabel) {
    var ident = getIdentifier();
    mod.allFields();
    localStorage.setItem(ident + "_searchIndex", index);
    localStorage.setItem(ident + "_searchIndexLabel", indexLabel);
    searchIndex().val(index);
    searchIndexLabel().html(indexLabel);
    [searchAll(), searchIndex(), searchIndexLabel(), searchInvert().parent()].forEach(function (x) {
      x.show();
    });
    return mod;
  };

  mod.indexInverse = function (index, indexLabel) {
    var ident = getIdentifier();
    mod.indexOnly(index, indexLabel);
    searchInvert().attr('checked', true);
    localStorage.setItem(ident + "_searchInvert", true);
    return mod;
  };

  mod.getSearch = function () {
    var query = searchTerm().val();
    var url = "documents/search?q=" + window.encodeURIComponent(query);
    var field = searchFields().val();
    var exclude = searchExclude().is(':checked');
    var invert = searchInvert().is(':checked');
    var index = searchIndex().val();
    var fieldlabels = fieldLabels();

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
        var label = fieldlabels[$(item).attr('data-field-field')].join(": ");
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

  mod.removeField = function (t) {
    var ident = getIdentifier();
    var searchFields = localStorage.getItem(ident + "_searchFields");
    var newSearchFields;
    var fields = JSON.parse(searchFields);
    var newFields;
    var id = $(t).attr("data-field-field");

    if (fields !== null) {
      newFields = fields.filter(function (x) {
        return x !== id;
      });
      newSearchFields = JSON.stringify(newFields);
      localStorage.setItem(ident + "_searchFields", (newFields.length === 0) ? null : newSearchFields);
      localStorage.setItem(ident + "_searchIndex", null);
      mod.loadSearchVals();
    }

    return mod;
  };

  mod.addField = function (t) {
    var ident = getIdentifier();
    var searchFields = localStorage.getItem(ident + "_searchFields");
    var newSearchFields;
    var fields = JSON.parse(searchFields);
    var newFields;
    var id = $(t).attr("data-field-field");

    if (fields === null) {
      fields = [];
    }

    newFields = fields.concat(id);
    newSearchFields = JSON.stringify(newFields);
    localStorage.setItem(ident + "_searchFields", (newFields.length === 0) ? null : newSearchFields);
    localStorage.setItem(ident + "_searchIndex", null);
    mod.loadSearchVals();

    return mod;
  };

  mod.addIndex = function () {
    var val = indexVal();
    var ident = getIdentifier();

    if (val) {
      localStorage.setItem(ident + "_searchFields", null);
      localStorage.setItem(ident + "_searchIndex", val);
      localStorage.setItem(ident + "_searchIndexLabel", $("option[value=" + val + "]").html());
      mod.loadSearchVals();
    }

    return mod;
  };

  mod.toggleInversion = function () {
    var ident = getIdentifier();
    localStorage.setItem(ident + "_searchInvert", maybeTrue(searchInvert().is(":checked")));
    localStorage.setItem(ident + "_searchExclude", null);
    mod.loadSearchVals();

    return mod;
  };

  mod.toggleExclusion = function () {
    var ident = getIdentifier();
    localStorage.setItem(ident + "_searchExclude", maybeTrue(searchExclude().is(":checked")));
    localStorage.getItem(ident + "_searchInvert", null);
    mod.loadSearchVals();

    return mod;
  };

  mod.loadSearchVals = function () {
    var ident = getIdentifier();
    var exclude = localStorage.getItem(ident + "_searchExclude");
    var invert = localStorage.getItem(ident + "_searchInvert");
    var index = localStorage.getItem(ident + "_searchIndex");
    var fieldids = localStorage.getItem(ident + "_searchFields");
    var fields;
    var indexLabel;
    var params = [exclude, invert, index, fieldids].map(function (x) {
      return (x === "null" || x === "false" || x === "true") ? JSON.parse(x) : x;
    });
    var allNull = params.every(function (x) {
      return x === null;
    });

    try {
      if (allNull) {
        mod.allFields();
      } else if (params[0] === true) {
        fields = JSON.parse(fieldids);
        mod.excludedFields(fields);
      } else if (params[1] === null && params[3] !== null) {
        fields = JSON.parse(fieldids);
        if (fields.length > 1) {
          mod.multipleFields(fields);
        } else {
          mod.singleField(fields);
        }
      } else if (params[3] !== null) {
        fields = JSON.parse(fieldids);
        if (fields.length > 1) {
          mod.multipleFields(fields);
        } else {
          mod.singleFieldInverse(fields);
        }
      } else if (params[1] === null) {
        indexLabel = localStorage.getItem(ident + "_searchIndexLabel");
        mod.indexOnly(index, indexLabel);
      } else if (params[1] === true) {
        indexLabel = localStorage.getItem(ident + "_searchIndexLabel");
        mod.indexInverse(index, indexLabel);
      }
    } catch (e) {
      window.console.log(e);
      mod.allFields();
    }

    return mod;
  };

  mod.toggleSelection = function (t) {
    var target = $(t);

    if (target.is(":checked")) {
      target.next("label").next("table").addClass("selected-for-save");
    } else {
      target.next("label").next("table").removeClass("selected-for-save");
    }

    return mod;
  };

  return mod;
})();