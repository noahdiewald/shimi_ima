// # The search user interface
//
// *Implicit depends:* DOM, JQuery
//
// Handles the search user interface.

// Variable Definitions

var utils = require('../utils.js');
var sets = require('../sets.js');
var setsui = require('./setsui.js');
var documents = require('./documents.js');
var multipleFields;
var loadSearchVals;

// Internal functions

// User interface element
var searchIndex = function ()
{
  'use strict';

  return $('#document-search-index');
};

// User interface element
var searchIndexLabel = function ()
{
  'use strict';

  return $('#search-index-label');
};

// User interface element
var searchTerm = function ()
{
  'use strict';

  return $('#document-search-term');
};

// User interface element
var searchFields = function ()
{
  'use strict';

  return $('#document-search-field');
};

// User interface element
var searchFieldsLabel = function ()
{
  'use strict';

  return $('#search-field-label');
};

// User interface element
var searchExclude = function ()
{
  'use strict';

  return $('#document-search-exclude');
};

// User interface element
var searchInvert = function ()
{
  'use strict';

  return $('#document-search-invert');
};

// User interface element
var searchAll = function ()
{
  'use strict';

  return $('#search-all-fields-switch');
};

// User interface element
var searchListing = function ()
{
  'use strict';

  return $('#search-listing');
};

// User interface element
var getIdentifier = function ()
{
  'use strict';

  return documents.identifier();
};

// All the form elements.
var formElems = [searchIndex, searchIndexLabel, searchFields, searchFieldsLabel, searchExclude, searchInvert, searchAll];

// If searching a user created index, the value of the hidden input
// where the index id specified.
var indexVal = function ()
{
  'use strict';

  var val = $('#index-index-input').val();
  if (val.length === 0)
  {
    return null;
  }
  else
  {
    return val;
  }
};

// Used for values that must either be true or null.
var maybeTrue = function (bool)
{
  'use strict';

  if (bool)
  {
    return true;
  }
  else
  {
    return null;
  }
};

// Clear all search information that is stored in local storage.
var clearStore = function ()
{
  'use strict';

  var ident = getIdentifier();
  localStorage.setItem(ident + '_searchIndex', null);
  localStorage.setItem(ident + '_searchIndexLabel', null);
  localStorage.setItem(ident + '_searchFields', null);
  localStorage.setItem(ident + '_searchExclude', null);
  localStorage.setItem(ident + '_searchInvert', null);
};

// Clear the search form.
var clearVals = function ()
{
  'use strict';

  formElems.forEach(function (x)
  {
    var elem = x();
    switch (elem.attr('type'))
    {
    case 'hidden':
      elem.val('');
      break;
    case 'checkbox':
      elem.prop('checked', false);
      break;
    }
  });
};

// Hide all the form elements.
var hideElems = function ()
{
  'use strict';

  formElems.forEach(function (x)
  {
    var elem = x();
    switch (elem.attr('type'))
    {
    case 'hidden':
      break;
    case 'checkbox':
      elem.parent('div').hide();
      break;
    default:
      elem.hide();
    }
  });
};

// Get the field labels from session storage.
var fieldLabels = function ()
{
  'use strict';

  var ident = getIdentifier();
  var fieldlabels = JSON.parse(sessionStorage.getItem(ident + '_labels'));
  return fieldlabels;
};

// Render the search field item template using given values.
var searchFieldItem = function (field, fieldLabel)
{
  'use strict';

  return templates['search-field-item'].render(
  {
    fieldLabel: fieldLabel,
    field: field
  });
};

// Set the fields to search.
var setFields = function (fields)
{
  'use strict';

  var fLabels = fieldLabels();
  var jFields = JSON.stringify(fields);
  var sfls = searchFieldsLabel();
  var ident = getIdentifier();

  searchFields().val(jFields);
  localStorage.setItem(ident + '_searchFields', jFields);

  var linkLabels = fields.map(function (x)
  {
    return searchFieldItem(x, fLabels[x].join(': '));
  });

  sfls.html(linkLabels.join(' '));

  return true;
};

// Exported functions

// Put the form in a state where all fields will be searched.
var allFields = function ()
{
  'use strict';

  clearStore();
  hideElems();
  clearVals();
  return true;
};

// Put the form in a state where one field will be searched.
var singleField = function (fields)
{
  'use strict';

  multipleFields(fields);
  searchInvert().parent().show();
  return true;
};

// Put the form in a state where one field will be used to perform an
// inverse search.
var singleFieldInverse = function (fields)
{
  'use strict';

  var ident = getIdentifier();
  singleField(fields);
  searchInvert().prop('checked', true);
  localStorage.setItem(ident + '_searchInvert', true);
  return true;
};

// Put the form in a state where multiple fields will be searched.
multipleFields = function (fields)
{
  'use strict';

  allFields();
  setFields(fields);
  [searchAll(), searchFieldsLabel(), searchExclude().parent()].forEach(function (x)
  {
    x.show();
  });
  return true;
};

// Put the form in a state where fields will be excluded from search.
var excludedFields = function (fields)
{
  'use strict';

  var ident = getIdentifier();
  if (fields.length > 1)
  {
    multipleFields(fields);
  }
  else
  {
    singleField(fields);
  }
  searchExclude().prop('checked', true);
  localStorage.setItem(ident + '_searchExclude', true);
  return true;
};

// Put the form in a state where a user created index will be searched.
var indexOnly = function (index, indexLabel)
{
  'use strict';

  var ident = getIdentifier();
  allFields();
  localStorage.setItem(ident + '_searchIndex', index);
  localStorage.setItem(ident + '_searchIndexLabel', indexLabel);
  searchIndex().val(index);
  searchIndexLabel().html(indexLabel);
  [searchAll(), searchIndex(), searchIndexLabel(), searchInvert().parent()].forEach(function (x)
  {
    x.show();
  });
  return true;
};

// Put the form in a state where a user created index will be used to
// perform an inverse search.
var indexInverse = function (index, indexLabel)
{
  'use strict';

  var ident = getIdentifier();
  indexOnly(index, indexLabel);
  searchInvert().prop('checked', true);
  localStorage.setItem(ident + '_searchInvert', true);
  return true;
};

// Perform the search.
var getSearch = function ()
{
  'use strict';

  var query = searchTerm().val();
  var url = 'documents/search?q=' + window.encodeURIComponent(query);
  var field = searchFields().val();
  var exclude = searchExclude().is(':checked');
  var invert = searchInvert().is(':checked');
  var index = searchIndex().val();
  var fieldlabels = fieldLabels();

  if (index)
  {
    url = url + '&index=' + index;
  }
  else
  {
    if (field)
    {
      url = url + '&field=' + field;
    }
    if (exclude)
    {
      url = url + '&exclude=true';
    }
  }
  if (invert)
  {
    url = url + '&invert=true';
  }

  searchListing().hide();

  $.get(url, function (searchResults)
  {
    searchListing().html(searchResults);
    $('.search-result-field-id').each(function (index, item)
    {
      var label = fieldlabels[$(item).attr('data-field-field')].join(': ');
      var target = $(item).children('a').first();
      target.html(label);
      target.attr('data-search-label', label);
    });
    if (!invert)
    {
      $('.search-results th').each(function (index, item)
      {
        var itemText = $.trim($(item).children('a').html());
        var re = new RegExp('(' + query + ')', 'g');
        var newText = itemText.replace(re, '<span class="highlight">$1</span>');
        $(item).children('a').html(newText);
      });
    }
    searchListing().show();
  });

  return true;
};

// Remove a field from those that will be searched (or excluded in an
// exclusive search.)
var removeField = function (t)
{
  'use strict';

  var ident = getIdentifier();
  var searchFields = localStorage.getItem(ident + '_searchFields');
  var newSearchFields;
  var fields = JSON.parse(searchFields);
  var newFields;
  var id = $(t).attr('data-field-field');

  if (fields !== null)
  {
    newFields = fields.filter(function (x)
    {
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
var addField = function (t)
{
  'use strict';

  var ident = getIdentifier();
  var searchFields = localStorage.getItem(ident + '_searchFields');
  var newSearchFields;
  var fields = JSON.parse(searchFields);
  var newFields;
  var id = $(t).attr('data-field-field');

  if (fields === null)
  {
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
var addIndex = function ()
{
  'use strict';

  var val = indexVal();
  var ident = getIdentifier();

  if (val)
  {
    localStorage.setItem(ident + '_searchFields', null);
    localStorage.setItem(ident + '_searchIndex', val);
    localStorage.setItem(ident + '_searchIndexLabel', $('option[value=' + val + ']').html());
    loadSearchVals();
  }

  return true;
};

// Toggle the inverse search setting.
var toggleInversion = function ()
{
  'use strict';

  var ident = getIdentifier();
  localStorage.setItem(ident + '_searchInvert', maybeTrue(searchInvert().is(':checked')));
  localStorage.setItem(ident + '_searchExclude', null);
  loadSearchVals();

  return true;
};

// Toggle the exclusive search setting.
var toggleExclusion = function ()
{
  'use strict';

  var ident = getIdentifier();
  localStorage.setItem(ident + '_searchExclude', maybeTrue(searchExclude().is(':checked')));
  localStorage.getItem(ident + '_searchInvert', null);
  loadSearchVals();

  return true;
};

// The functions that alter the search form above store the values in
// local storage. This interprets those values and puts the search form
// in a consistent state.
loadSearchVals = function ()
{
  'use strict';

  var ident = getIdentifier();
  var exclude = localStorage.getItem(ident + '_searchExclude');
  var invert = localStorage.getItem(ident + '_searchInvert');
  var index = localStorage.getItem(ident + '_searchIndex');
  var fieldids = localStorage.getItem(ident + '_searchFields');
  var fields;
  var indexLabel;
  var params = [exclude, invert, index, fieldids].map(function (x)
  {
    return (x === 'null' || x === 'false' || x === 'true') ? JSON.parse(x) : x;
  });
  var allNull = params.every(function (x)
  {
    return x === null;
  });

  try
  {
    if (allNull)
    {
      allFields();
    }
    else if (params[0] === true)
    {
      fields = JSON.parse(fieldids);
      excludedFields(fields);
    }
    else if (params[1] === null && params[3] !== null)
    {
      fields = JSON.parse(fieldids);
      if (fields.length > 1)
      {
        multipleFields(fields);
      }
      else
      {
        singleField(fields);
      }
    }
    else if (params[3] !== null)
    {
      fields = JSON.parse(fieldids);
      if (fields.length > 1)
      {
        multipleFields(fields);
      }
      else
      {
        singleFieldInverse(fields);
      }
    }
    else if (params[1] === null)
    {
      indexLabel = localStorage.getItem(ident + '_searchIndexLabel');
      indexOnly(index, indexLabel);
    }
    else if (params[1] === true)
    {
      indexLabel = localStorage.getItem(ident + '_searchIndexLabel');
      indexInverse(index, indexLabel);
    }
  }
  catch (e)
  {
    window.console.log(e);
    allFields();
  }

  return true;
};

// Toggle selection of result to save to set.
var toggleSelection = function (t)
{
  'use strict';

  var target = $(t);

  if (target.is(':checked'))
  {
    target.next('label').next('table').addClass('selected-for-save');
  }
  else
  {
    target.next('label').next('table').removeClass('selected-for-save');
  }

  return true;
};

exports(allFields);
exports(singleField);
exports(singleFieldInverse);
exports(multipleFields);
exports(excludedFields);
exports(indexOnly);
exports(indexInverse);
exports(getSearch);
exports(removeField);
exports(addField);
exports(addIndex);
exports(toggleInversion);
exports(toggleExclusion);
exports(loadSearchVals);
exports(toggleSelection);
