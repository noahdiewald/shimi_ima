// # Change Event Handling
//
// *Implicit depends:* DOM
//
// Like [`click-dispatch.js`](./click-dispatch.html) I would like
// to centralize the 'change' events. This is a start and a bit of
// an experiment to isolate and route input.

// ## Variable Definitions

var S = require('sender');
var searchui = require('documents/searchui');
var newDialog = require('index_tool/new-dialog');

// ## Exported Functions

// Run to add event listeners to `document`.
var changes = function () {
  'use strict';

  var changeTargets = [];

  // ### Document Index

  changeTargets['index-filter-form select'] = function () {
    S.sender('documents-altered');
  };

  // ### Search UI Change Events

  changeTargets['document-search-exclude'] = function (e) {
    searchui.toggleExclusion();

    return e;
  };

  changeTargets['document-search-invert'] = function (e) {
    searchui.toggleInversion();

    return e;
  };

  // ### New index

  changeTargets['index-doctype-input'] = function (e) {
    newDialog.doctypeInputChange();

    return e;
  };

  changeTargets['index-fieldset-input'] = function (e) {
    newDialog.fieldsetInputChange();

    return e;
  };

  document.onchange = function (e) {
    if (e.target && changeTargets[e.target.id]) {
      changeTargets[e.target.id](e);
    }

    return e;
  };

  return document;
};

exports.changes = changes;
