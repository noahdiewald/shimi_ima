// # The search user interface
//
// *Implicit depends:* DOM, JQuery
//
// Handles the search user interface.

// ## Variable Definitions

// ### Imported Modules

var templates = require('templates');
var utils = require('utils');
var sets = require('sets');
var setsui = require('documents/setsui');
var ui = require('documents/ui-shared');
var info = require('documents/information');
var ajax = require('ajax');

// ### Function Names

var multipleFields;
var loadSearchVals;

// ## Internal functions

// User interface element
var searchIndex = function () {
  'use strict';

  return document.getElementById('document-search-index');
};

// User interface element
var searchIndexLabel = function () {
  'use strict';

  return document.getElementById('search-index-label');
};

// User interface element
var searchTerm = function () {
  'use strict';

  return document.getElementById('document-search-term');
};

// User interface element
var searchFields = function () {
  'use strict';

  return document.getElementById('document-search-field');
};

// User interface element
var searchFieldsLabel = function () {
  'use strict';

  return document.getElementById('search-field-label');
};

// User interface element
var searchExclude = function () {
  'use strict';

  return document.getElementById('document-search-exclude');
};

// User interface element
var searchInvert = function () {
  'use strict';

  return document.getElementById('document-search-invert');
};

// User interface element
var searchAll = function () {
  'use strict';

  return document.getElementById('search-all-fields-switch');
};

// User interface element
var searchListing = function () {
  'use strict';

  return document.getElementById('search-listing');
};

// User interface element
var getIdentifier = function () {
  'use strict';

  return info.identifier();
};

// All the form elements.
var formElems = [searchIndex, searchIndexLabel, searchFields, searchFieldsLabel, searchExclude, searchInvert, searchAll];

// If searching a user created index, the value of the hidden input
// where the index id specified.
var indexVal = function () {
  'use strict';

  var val = ui.indexIndexInput().value;

  if (val.length === 0) {
    return null;
  } else {
    return val;
  }
};

// Used for values that must either be true or null.
var maybeTrue = function (bool) {
  'use strict';

  if (bool) {
    return true;
  } else {
    return null;
  }
};

// Clear all search information that is stored in local storage.
var clearStore = function () {
  'use strict';

  var ident = getIdentifier();
  localStorage.setItem(ident + '_searchIndex', null);
  localStorage.setItem(ident + '_searchIndexLabel', null);
  localStorage.setItem(ident + '_searchFields', null);
  localStorage.setItem(ident + '_searchExclude', null);
  localStorage.setItem(ident + '_searchInvert', null);
};

// Do something with the formElems base on their type.
var forEachInputType = function (hidden, checkbox, def) {
  'use strict';

  formElems.forEach(function (x) {
    var elem = x();

    if (elem) {
      switch (elem.getAttribute('type')) {
      case 'hidden':
        hidden(elem);
        break;
      case 'checkbox':
        checkbox(elem);
        break;
      default:
        def(elem);
      }
    }
  });
};

// Clear the search form.
var clearVals = function () {
  'use strict';

  forEachInputType(function (elem) {
    elem.value = '';
  }, function (elem) {
    elem.checked = false;
  }, function () {
    return;
  });
};

// Hide all the form elements.
var hideElems = function () {
  'use strict';

  forEachInputType(function () {
    return;
  }, function (elem) {
    ui.hide(elem.parentElement);
  }, function (elem) {
    ui.hide(elem);
  });
};

// Get the field labels from session storage.
var fieldLabels = function () {
  'use strict';

  var ident = getIdentifier();
  var fieldlabels = JSON.parse(sessionStorage.getItem(ident + '_labels'));
  return fieldlabels;
};

// Render the search field item template using given values.
var searchFieldItem = function (field, fieldLabel) {
  'use strict';

  return templates['search-field-item']({
    fieldLabel: fieldLabel,
    field: field
  });
};

// Set the fields to search.
var setFields = function (fields) {
  'use strict';

  var fLabels = fieldLabels();
  var jFields = JSON.stringify(fields);
  var sfls = searchFieldsLabel();
  var ident = getIdentifier();

  searchFields().value = jFields;
  localStorage.setItem(ident + '_searchFields', jFields);

  var linkLabels = fields.map(function (x) {
    return searchFieldItem(x, fLabels[x].join(': '));
  });

  sfls.innerHTML = linkLabels.join(' ');

  return true;
};

// Exported functions

// Put the form in a state where all fields will be searched.
var allFields = function () {
  'use strict';

  clearStore();
  hideElems();
  clearVals();

  return true;
};

// Put the form in a state where one field will be searched.
var singleField = function (fields) {
  'use strict';

  multipleFields(fields);
  ui.show(searchInvert().parentElement);

  return true;
};

// Put the form in a state where one field will be used to perform an
// inverse search.
var singleFieldInverse = function (fields) {
  'use strict';

  var ident = getIdentifier();
  singleField(fields);
  searchInvert().checked = true;
  localStorage.setItem(ident + '_searchInvert', true);

  return true;
};

// Put the form in a state where multiple fields will be searched.
multipleFields = function (fields) {
  'use strict';

  allFields();
  setFields(fields);
  [searchAll(), searchFieldsLabel(), searchExclude().parentElement].forEach(function (x) {
    ui.show(x);
  });

  return true;
};

// Put the form in a state where fields will be excluded from search.
var excludedFields = function (fields) {
  'use strict';

  var ident = getIdentifier();

  if (fields.length > 1) {
    multipleFields(fields);
  } else {
    singleField(fields);
  }

  searchExclude().checked = true;
  localStorage.setItem(ident + '_searchExclude', true);

  return true;
};

// Put the form in a state where a user created index will be searched.
var indexOnly = function (index, indexLabel) {
  'use strict';

  var ident = getIdentifier();

  allFields();
  localStorage.setItem(ident + '_searchIndex', index);
  localStorage.setItem(ident + '_searchIndexLabel', indexLabel);
  searchIndex().value = index;
  searchIndexLabel().innerHTML = indexLabel;

  [searchAll(), searchIndex(), searchIndexLabel(), searchInvert().parentElement].forEach(function (x) {
    ui.show(x);
  });

  return true;
};

// Put the form in a state where a user created index will be used to
// perform an inverse search.
var indexInverse = function (index, indexLabel) {
  'use strict';

  var ident = getIdentifier();

  indexOnly(index, indexLabel);
  searchInvert().checked = true;
  localStorage.setItem(ident + '_searchInvert', true);

  return true;
};

// Perform the search.
var getSearch = function () {
  'use strict';

  var query = searchTerm().value;
  var url = 'documents/search?q=' + window.encodeURIComponent(query);
  var field = searchFields().value;
  var exclude = searchExclude().checked;
  var invert = searchInvert().checked;
  var index = searchIndex().value;
  var fieldlabels = fieldLabels();

  if (index) {
    url = url + '&index=' + index;
  } else {
    if (field) {
      url = url + '&field=' + field;
    }
    if (exclude) {
      url = url + '&exclude=true';
    }
  }
  if (invert) {
    url = url + '&invert=true';
  }

  ui.hide(searchListing());

  ajax.get(url, function (req) {
    var results = req.response;
    var html;

    results.are_results = results.rows && results.rows.length > 0;

    if (results.index_listing) {
      results.rows = results.rows.map(function (row) {
        row.key = row.key.map(function (x) {
          return x[1];
        }).join(',');

        return row;
      });
    }

    html = templates['document-search'](results);
    searchListing().innerHTML = html;

    Array.prototype.forEach.call(document.getElementsByClassName('search-result-field-id'), function (item) {
      var label = fieldlabels[item.dataset.fieldField].join(': ');
      var target = item.children[0];

      target.innerHTML = label;
      target.dataset.searchLabel = label;
    });

    if (!invert) {
      Array.prototype.forEach.call(document.querySelectorAll('.search-results th'), function (item) {
        var itemText = item.children[0].innerHTML.replace(/(^\s|\s$)/g, '');
        var re = new RegExp('(' + query + ')', 'g');
        var newText = itemText.replace(re, '<span class="highlight">$1</span>');

        item.children[0].innerHTML = newText;
      });
    }

    ui.show(searchListing());
  });

  return true;
};

// Remove a field from those that will be searched (or excluded in an
// exclusive search.)
var removeField = function (t) {
  'use strict';

  var ident = getIdentifier();
  var searchFields = localStorage.getItem(ident + '_searchFields');
  var newSearchFields;
  var fields = JSON.parse(searchFields);
  var newFields;
  var id = t.dataset.fieldField;

  if (fields !== null) {
    newFields = fields.filter(function (x) {
      return x !== id;
    });
    newSearchFields = JSON.stringify(newFields);
    localStorage.setItem(ident + '_searchFields', (newFields.length === 0) ? null : newSearchFields);
    localStorage.setItem(ident + '_searchIndex', null);
    loadSearchVals();
  }

  return true;
};

// Add a field to those that will be searched (or excluded in an
// exclusive search.)
var addField = function (t) {
  'use strict';

  var ident = getIdentifier();
  var searchFields = localStorage.getItem(ident + '_searchFields');
  var newSearchFields;
  var fields = JSON.parse(searchFields);
  var newFields;
  var id = t.dataset.fieldField;

  if (fields === null) {
    fields = [];
  }

  newFields = sets.union(fields, id);
  newSearchFields = JSON.stringify(newFields);
  localStorage.setItem(ident + '_searchFields', (newFields.length === 0) ? null : newSearchFields);
  localStorage.setItem(ident + '_searchIndex', null);
  loadSearchVals();

  return true;
};

// Add a user created index to be searched.
var addIndex = function () {
  'use strict';

  var val = indexVal();
  var ident = getIdentifier();

  if (val) {
    localStorage.setItem(ident + '_searchFields', null);
    localStorage.setItem(ident + '_searchIndex', val);
    localStorage.setItem(ident + '_searchIndexLabel', $('option[value=' + val + ']').html());
    loadSearchVals();
  }

  return true;
};

// Toggle the inverse search setting.
var toggleInversion = function () {
  'use strict';

  var ident = getIdentifier();

  localStorage.setItem(ident + '_searchInvert', maybeTrue(searchInvert().checked));
  localStorage.setItem(ident + '_searchExclude', null);
  loadSearchVals();

  return true;
};

// Toggle the exclusive search setting.
var toggleExclusion = function () {
  'use strict';

  var ident = getIdentifier();

  localStorage.setItem(ident + '_searchExclude', maybeTrue(searchExclude().checked));
  localStorage.getItem(ident + '_searchInvert', null);
  loadSearchVals();

  return true;
};

// The functions that alter the search form above store the values in
// local storage. This interprets those values and puts the search form
// in a consistent state.
loadSearchVals = function () {
  'use strict';

  var ident = getIdentifier();
  var exclude = localStorage.getItem(ident + '_searchExclude');
  var invert = localStorage.getItem(ident + '_searchInvert');
  var index = localStorage.getItem(ident + '_searchIndex');
  var fieldids = localStorage.getItem(ident + '_searchFields');
  var fields;
  var indexLabel;
  var params = [exclude, invert, index, fieldids].map(function (x) {
    return (x === 'null' || x === 'false' || x === 'true') ? JSON.parse(x) : x;
  });
  var allNull = params.every(function (x) {
    return x === null;
  });

  try {
    if (allNull) {
      allFields();
    } else if (params[0] === true) {
      fields = JSON.parse(fieldids);
      excludedFields(fields);
    } else if (params[1] === null && params[3] !== null) {
      fields = JSON.parse(fieldids);
      if (fields.length > 1) {
        multipleFields(fields);
      } else {
        singleField(fields);
      }
    } else if (params[3] !== null) {
      fields = JSON.parse(fieldids);
      if (fields.length > 1) {
        multipleFields(fields);
      } else {
        singleFieldInverse(fields);
      }
    } else if (params[1] === null) {
      indexLabel = localStorage.getItem(ident + '_searchIndexLabel');
      indexOnly(index, indexLabel);
    } else if (params[1] === true) {
      indexLabel = localStorage.getItem(ident + '_searchIndexLabel');
      indexInverse(index, indexLabel);
    }
  } catch (e) {
    allFields();
  }

  return true;
};

// Toggle selection of result to save to set.
var toggleSelection = function (target) {
  'use strict';

  if (target.checked) {
    target.nextSibling.nextSibling.classList.add('selected-for-save');
  } else {
    target.nextSibling.nextSibling.classList.remove('selected-for-save');
  }

  return true;
};

exports.addField = addField;
exports.addIndex = addIndex;
exports.allFields = allFields;
exports.excludedFields = excludedFields;
exports.getSearch = getSearch;
exports.indexInverse = indexInverse;
exports.indexOnly = indexOnly;
exports.loadSearchVals = loadSearchVals;
exports.multipleFields = multipleFields;
exports.removeField = removeField;
exports.singleField = singleField;
exports.singleFieldInverse = singleFieldInverse;
exports.toggleExclusion = toggleExclusion;
exports.toggleInversion = toggleInversion;
exports.toggleSelection = toggleSelection;
