// # The sets user interface
//
// *Implicit depends:* DOM, JQuery
//
// Handles the sets user interface.

// Variable Definitions

var templates = require('templates');
var S = require('../sender.js');
var form = require('form');
var flash = require('flash');
var sets = require('sets');
var utils = require('utils');
var info = require('documents/information');
var removeSet;
var setSets;
var selectedElementsToArray;
var selectedSaveResultsToArray;
var render;
var getSets;
var getSet;

// Internal functions

// Generic map function
var map = function (x, y) {
  'use strict';

  return Array.prototype.map.call(x, y);
};

// User interface element
var selectedSearchResults = function () {
  'use strict';

  return document.querySelectorAll('input.select-results:checked + label + table tr');
};

// User interface element
var selectedSetResults = function () {
  'use strict';

  return document.querySelectorAll('input.set-element-selection:checked');
};

// User interface element
var nameInput = function () {
  'use strict';

  return document.getElementById('new-set-input');
};

// User interface element
var targetInput = function () {
  'use strict';

  return document.getElementById('new-set-target-input');
};

// User interface element
var newSetDialog = function () {
  'use strict';

  return document.getElementById('new-set-dialog');
};

// User interface element
var setA = function () {
  'use strict';

  return document.getElementById('document-set-a-input');
};

// User interface element
var setB = function () {
  'use strict';

  return document.getElementById('document-set-b-input');
};

// User interface element
var worksheetsSet = function () {
  'use strict';

  return document.getElementById('document-worksheets-set-input');
};

// User interface element
var op = function () {
  'use strict';

  return document.getElementById('document-set-operation-input');
};

// User interface element
var setListing = function () {
  'use strict';

  return document.getElementById('set-listing');
};

// User interface element
var sessionKey = function () {
  'use strict';

  return info.setsKey();
};

// Custom member function to use with [sets.js](./sets.html).
var member = function (arr, x) {
  'use strict';

  return arr.some(function (y) {
    return x[0] === y[0] && x[1] === y[1];
  });
};

// Ensure that the set is correct.
var processSet = function (set) {
  'use strict';

  var name = set [0];
  var arr = sets.unique(set [1], member);
  var procSet = [name, arr];
  return procSet;
};

// Perform the union of the sets specified by the user interface.
var union = function (setNameA, setNameB) {
  'use strict';

  var setElemsA = getSet(setNameA)[1];
  var setElemsB = getSet(setNameB)[1];
  var newSet = sets.union(setElemsA, setElemsB, member);
  render(newSet);
  return true;
};

// Perform the intersection of the sets specified by the user interface.
var intersection = function (setNameA, setNameB) {
  'use strict';

  var setElemsA = getSet(setNameA)[1];
  var setElemsB = getSet(setNameB)[1];
  var newSet = sets.intersection(setElemsA, setElemsB, member);
  render(newSet);
  return true;
};

// Perform the relative complement of the sets specified by the user
// interface.
var relativeComplement = function (setName1, setName2) {
  'use strict';

  var setElems1 = getSet(setName1)[1];
  var setElems2 = getSet(setName2)[1];
  var newSet = sets.relativeComplement(setElems1, setElems2, member);
  render(newSet);

  return true;
};

// Perform the symmetric difference of the sets specified by the user
// interface.
var symmetricDifference = function (setNameA, setNameB) {
  'use strict';

  var setElemsA = getSet(setNameA)[1];
  var setElemsB = getSet(setNameB)[1];
  var newSet = sets.symmetricDifference(setElemsA, setElemsB, member);
  render(newSet);

  return true;
};

// Get the sets saved in session storage
getSets = function () {
  'use strict';

  var curr = window.sessionStorage.getItem(sessionKey());
  var retval = [];

  if (curr !== null) {
    retval = JSON.parse(curr);
  }

  return retval;
};

// View a set.
var view = function (setName) {
  'use strict';

  var elems = getSet(setName)[1];
  render(elems);

  return true;
};

// Remove a set.
var remove = function (setName) {
  'use strict';

  removeSet(setName);
  render([]);
  S.sender('sets-changed');

  return true;
};

// Perform set removal.
removeSet = function (setName) {
  'use strict';

  var nnew;
  var curr = getSets();
  nnew = curr.filter(function (x) {
    return x[0] !== setName;
  });
  setSets(nnew);

  return true;
};

// Retrieve the set names.
var getSetNames = function () {
  'use strict';

  var curr = getSets();
  return curr.map(function (x) {
    return x[0];
  });
};

// Save sets to session storage.
setSets = function (nnew) {
  'use strict';

  var procSets;
  if (Array.isArray(nnew)) {
    procSets = nnew.map(function (x) {
      return processSet(x);
    });
    window.sessionStorage.setItem(sessionKey(), JSON.stringify(procSets));
  } else {
    window.sessionStorage.settem(sessionKey(), '[]');
  }

  return true;
};

// Save a set to session storage.
var setSet = function (nnew) {
  'use strict';

  if (Array.isArray(nnew) && nnew.length === 2) {
    var curr = getSets();
    var newName = nnew[0];
    var filtered = curr.filter(function (x) {
      return x[0] !== newName;
    });
    setSets(filtered.concat([nnew]));
  }
  return true;
};

// Convert selected search results or a selected elements to an array.
var selectedToArray = function (target) {
  'use strict';

  var retval = [];

  switch (target) {
  case 'search':
    retval = selectedSaveResultsToArray();
    break;
  case 'sets':
    retval = selectedElementsToArray();
    break;
  }

  return retval;
};

// Convert selected elements to an array.
selectedElementsToArray = function () {
  'use strict';

  return map(selectedSetResults(), function (elem) {
    return [elem.dataset.context, elem.value];
  });
};

// Convert selected search results to an array.
selectedSaveResultsToArray = function () {
  'use strict';

  var retval;

  retval = map(selectedSearchResults(), function (elem) {
    var id = elem.querySelector('th a').getAttribute('href').replace(/^#/, '');
    var context = elem.querySelector('td.search-result-context a').innerHTML.trim();
    return [context, id];
  });

  return retval;
};

// Render the set for display.
render = function (setElems) {
  'use strict';

  var total = setElems.length;
  var elems = setElems.map(function (x) {
    return {
      id: x[1],
      context: x[0]
    };
  });
  var listing = templates['set-listing']({
    elements: elems,
    total: total
  });
  setListing().innerHTML = listing;
  return true;
};

// Exported functions

// Retrieve a set.
var getSet = function (setName) {
  'use strict';

  var retval;
  var curr = getSets();
  retval = curr.filter(function (x) {
    return x[0] === setName;
  })[0];

  return retval;
};

// Perform a set operation.
var performOp = function () {
  'use strict';

  switch (op().value) {
  case 'view-a':
    view(setA().value);
    break;
  case 'view-b':
    view(setB().value);
    break;
  case 'remove-a':
    remove(setA().value);
    break;
  case 'remove-b':
    remove(setB().value);
    break;
  case 'union':
    union(setA().value, setB().value);
    break;
  case 'intersection':
    intersection(setA().value, setB().value);
    break;
  case 'symmetric-difference':
    symmetricDifference(setA().value, setB().value);
    break;
  case 'relative-complement-b-in-a':
    relativeComplement(setA().value, setB().value);
    break;
  case 'relative-complement-a-in-b':
    relativeComplement(setB().value, setA().value);
    break;
  default:
    break;
  }
  return true;
};

// Update the selection of sets to choose from.
var updateSelection = function () {
  'use strict';

  var currNames = getSetNames();
  var newOptions = templates['set-options']({
    names: currNames
  });

  setA().innerHTML = newOptions;
  setB().innerHTML = newOptions;
  worksheetsSet().innerHTML = newOptions;

  return true;
};

// Display the dialog of saving new sets.
var displayNewSetDialog = function (target) {
  'use strict';

  targetInput().value = target;
  form.show(newSetDialog());

  return true;
};

// Save select items as a set.
var saveSelected = function () {
  'use strict';

  var name = nameInput().value;
  var selected;
  var newSet;

  if (!utils.isBlank(name)) {
    form.hide(newSetDialog());
    selected = selectedToArray(targetInput().value);
    newSet = [name, selected];
    setSet(newSet);
    nameInput().value = '';
    S.sender('sets-changed');
    flash.highlight('Success:', 'Set "' + name + '" saved.');
  } else {
    flash.error('Input invalid:', 'You must supply a valid name.');
  }

  return true;
};

// Toggle the selection of all elements.
var toggleSelectAll = function (target) {
  'use strict';

  if ($(target).is(':checked')) {
    $('input.set-element-selection').prop('checked', true);
  } else {
    $('input.set-element-selection').prop('checked', false);
  }
  return true;
};

exports.getSet = getSet;
exports.performOp = performOp;
exports.updateSelection = updateSelection;
exports.saveSelected = saveSelected;
exports.toggleSelectAll = toggleSelectAll;
exports.displayNewSetDialog = displayNewSetDialog;
